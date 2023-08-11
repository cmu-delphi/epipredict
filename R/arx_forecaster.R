#' Direct autoregressive forecaster with covariates
#'
#' This is an autoregressive forecasting model for
#' [epiprocess::epi_df] data. It does "direct" forecasting, meaning
#' that it estimates a model for a particular target horizon.
#'
#'
#' @param epi_data An `epi_df` object
#' @param outcome A character (scalar) specifying the outcome (in the
#'   `epi_df`).
#' @param predictors A character vector giving column(s) of predictor
#'   variables.
#' @param trainer A `{parsnip}` model describing the type of estimation.
#'   For now, we enforce `mode = "regression"`.
#' @param args_list A list of customization arguments to determine
#'   the type of forecasting model. See [arx_args_list()].
#'
#' @return A list with (1) `predictions` an `epi_df` of predicted values
#'   and (2) `epi_workflow`, a list that encapsulates the entire estimation
#'   workflow
#' @export
#' @seealso [arx_fcast_epi_workflow()], [arx_args_list()]
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value >= as.Date("2021-12-01"))
#'
#' out <- arx_forecaster(jhu, "death_rate",
#'   c("case_rate", "death_rate"))
#'
#' out <- arx_forecaster(jhu, "death_rate",
#'   c("case_rate", "death_rate"), trainer = quantile_reg(),
#'   args_list = arx_args_list(levels = 1:9 / 10))
arx_forecaster <- function(epi_data,
                           outcome,
                           predictors,
                           trainer = parsnip::linear_reg(),
                           args_list = arx_args_list()) {

  if (!is_regression(trainer))
    cli::cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'regression'.")

  wf <- arx_fcast_epi_workflow(
    epi_data, outcome, predictors, trainer, args_list
  )

  latest <- get_test_data(
    hardhat::extract_preprocessor(wf), epi_data, TRUE, args_list$nafill_buffer,
    args_list$forecast_date %||% max(epi_data$time_value)
  )

  wf <- generics::fit(wf, epi_data)
  preds <- predict(wf, new_data = latest) %>%
    tibble::as_tibble() %>%
    dplyr::select(-time_value)

  structure(list(
    predictions = preds,
    epi_workflow = wf,
    metadata = list(
      training = attr(epi_data, "metadata"),
      forecast_created = Sys.time()
    )),
    class = c("arx_fcast", "canned_epipred")
  )
}

#' Create a template `arx_forecaster` workflow
#'
#' This function creates an unfit workflow for use with [arx_forecaster()].
#' It is useful if you want to make small modifications to that forecaster
#' before fitting and predicting. Supplying a trainer to the function
#' may alter the returned `epi_workflow` object (e.g., if you intend to
#' use [quantile_reg()]) but can be omitted.
#'
#' @inheritParams arx_forecaster
#' @param trainer A `{parsnip}` model describing the type of estimation.
#'   For now, we enforce `mode = "regression"`. May be `NULL` (the default).
#'
#' @return An unfitted `epi_workflow`.
#' @export
#' @seealso [arx_forecaster()]
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value >= as.Date("2021-12-01"))
#'
#' arx_fcast_epi_workflow(jhu, "death_rate",
#'   c("case_rate", "death_rate"))
#'
#' arx_fcast_epi_workflow(jhu, "death_rate",
#'   c("case_rate", "death_rate"), trainer = quantile_reg(),
#'   args_list = arx_args_list(levels = 1:9 / 10))
arx_fcast_epi_workflow <- function(
    epi_data,
    outcome,
    predictors,
    trainer = NULL,
    args_list = arx_args_list()) {

  # --- validation
  validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_fcast", "alist")))
    cli::cli_abort("args_list was not created using `arx_args_list().")
  if (!(is.null(trainer) || is_regression(trainer)))
    cli::cli_abort("{trainer} must be a `{parsnip}` model of mode 'regression'.")
  lags <- arx_lags_validator(predictors, args_list$lags)
  scaled <- args_list$scaled
  scaled_frosting <- args_list$scaled_frosting

  # --- preprocessor
  r <- epi_recipe(epi_data)
  if (scaled) {
    # defaults
    scaling_by <- args_list$scaling_by
    scaling_variables <- args_list$scaling_variables
    scaling_df_pop_col <- args_list$scaling_df_pop_col
    scaling_rate <- args_list$scaling_rate
    scaling_df <- args_list$scaling_df
    if (is.null(scaling_by)) scaling_by <- c("geo_value" = "abbr")
    if (is.null(scaling_variables)) scaling_variables <- union(predictors, outcome)

    # change names to the scaled versions
    predictors <- map_chr(predictors, function(x) ifelse((x %in% scaling_variables), sprintf("%s_scaled", x), x))
    outcome <- map_chr(outcome, function(x) ifelse((x %in% scaling_variables), sprintf("%s_scaled", x), x))

    r <- r %>% step_population_scaling(
      !!!scaling_variables,
      df = scaling_df,
      df_pop_col = scaling_df_pop_col,
      by = scaling_by,
      rate_rescaling = scaling_rate,
      columns = args_list$scaling_columns,
    )
  }
  for (l in seq_along(lags)) {
    p <- predictors[l]
    r <- step_epi_lag(r, !!p, lag = lags[[l]])
  }
  r <- r %>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead) %>%
    step_epi_naomit() %>%
    step_training_window(n_recent = args_list$n_training)

  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  target_date <- args_list$target_date %||% forecast_date + args_list$ahead

  # --- postprocessor
  f <- frosting() %>% layer_predict() # %>% layer_naomit()
  if (scaled_frosting) {
    f <- f %>% layer_population_scaling(
      .pred,
      df = args_list$scaling_df,
      df_pop_col = scaling_df_pop_col,
      by = scaling_by,
      rate_rescaling = scaling_rate,
      create_new = FALSE,
    )
  }
  if (inherits(trainer, "quantile_reg")) {
    # add all levels to the forecaster and update postprocessor
    tau <- sort(compare_quantile_args(
      args_list$levels,
      rlang::eval_tidy(trainer$args$tau))
    )
    args_list$levels <- tau
    trainer$args$tau <- rlang::enquo(tau)
    f <- layer_quantile_distn(f, levels = tau) %>% layer_point_from_distn()
  } else {
    f <- layer_residual_quantiles(
      f, probs = args_list$levels, symmetrize = args_list$symmetrize,
      by_key = args_list$quantile_by_key)
  }
  f <- layer_add_forecast_date(f, forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)
  if (args_list$nonneg) f <- layer_threshold(f, dplyr::starts_with(".pred"))

  epi_workflow(r, trainer, f)
}


#' ARX forecaster argument constructor
#'
#' Constructs a list of arguments for [arx_forecaster()].
#'
#' @param lags Vector or List. Positive integers enumerating lags to use
#'   in autoregressive-type models (in days). By default, an unnamed list
#'   of lags will be set to correspond to the order of the predictors.
#' @param ahead Integer. Number of time steps ahead (in days) of the forecast
#'   date for which forecasts should be produced.
#' @param n_training Integer. An upper limit for the number of rows per
#'   key that are used for training
#'   (in the time unit of the `epi_df`).
#' @param forecast_date Date. The date on which the forecast is created.
#'   The default `NULL` will attempt to determine this automatically.
#' @param target_date Date. The date for which the forecast is intended.
#'   The default `NULL` will attempt to determine this automatically.
#' @param levels Vector or `NULL`. A vector of probabilities to produce
#'   prediction intervals. These are created by computing the quantiles of
#'   training residuals. A `NULL` value will result in point forecasts only.
#' @param symmetrize Logical. The default `TRUE` calculates
#'   symmetric prediction intervals. This argument only applies when
#'   residual quantiles are used. It is not applicable with
#'   `trainer = quantile_reg()`, for example.
#' @param nonneg Logical. The default `TRUE` enforces nonnegative predictions
#'   by hard-thresholding at 0.
#' @param quantile_by_key Character vector. Groups residuals by listed keys
#'   before calculating residual quantiles. See the `by_key` argument to
#'   [layer_residual_quantiles()] for more information. The default,
#'   `character(0)` performs no grouping. This argument only applies when
#'   residual quantiles are used. It is not applicable with
#'   `trainer = quantile_reg()`, for example.
#' @param nafill_buffer At predict time, recent values of the training data
#'   are used to create a forecast. However, these can be `NA` due to, e.g.,
#'   data latency issues. By default, any missing values will get filled with
#'   less recent data. Setting this value to `NULL` will result in 1 extra
#'   recent row (beyond those required for lag creation) to be used. Note that
#'   we require at least `min(lags)` rows of recent data per `geo_value` to
#'   create a prediction. For this reason, setting `nafill_buffer < min(lags)`
#'   will be treated as _additional_ allowed recent data rather than the
#'   total amount of recent data to examine.
#'
#'
#' @return A list containing updated parameter choices with class `arx_flist`.
#' @export
#'
#' @examples
#' arx_args_list()
#' arx_args_list(symmetrize = FALSE)
#' arx_args_list(levels = c(.1, .3, .7, .9), n_training = 120)
arx_args_list <- function(
    lags = c(0L, 7L, 14L),
    ahead = 7L,
    n_training = Inf,
    forecast_date = NULL,
    target_date = NULL,
    levels = c(0.05, 0.95),
    symmetrize = TRUE,
    nonneg = TRUE,
    quantile_by_key = character(0L),
    nafill_buffer = Inf,
    scaled = FALSE,
    scaled_frosting = NA,
    scaling_df = state_census,
    scaling_variables = NULL,
    scaling_df_pop_col = "pop",
    scaling_by = NULL,
    scaling_rate = 1,
    scaling_columns = NULL) {
  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)
  if (is.na(scaled_frosting)) scaled_frosting <- scaled # default to undoing the transformation
  arg_is_scalar(ahead, n_training, symmetrize, nonneg, scaling_rate)
  arg_is_chr(scaling_df_pop_col)
  arg_is_chr(quantile_by_key, allow_empty = TRUE)
  arg_is_chr(scaling_by, allow_null = TRUE)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_date(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, lags)
  arg_is_lgl(symmetrize, nonneg, scaled, scaled_frosting)
  arg_is_probabilities(levels, allow_null = TRUE)
  arg_is_pos(n_training, scaling_rate)
  if (is.finite(n_training)) arg_is_pos_int(n_training)
  if (is.finite(nafill_buffer)) arg_is_pos_int(nafill_buffer, allow_null = TRUE)

  max_lags <- max(lags)
  structure(
    enlist(
      lags = .lags,
      ahead,
      n_training,
      levels,
      forecast_date,
      target_date,
      symmetrize,
      nonneg,
      max_lags,
      quantile_by_key,
      nafill_buffer,
      scaled,
      scaled_frosting,
      scaling_df,
      scaling_variables,
      scaling_df_pop_col,
      scaling_by,
      scaling_rate
    ),
    class = c("arx_fcast", "alist")
  )
}


#' @export
print.arx_fcast <- function(x, ...) {
  name <- "ARX Forecaster"
  NextMethod(name = name, ...)
}

compare_quantile_args <- function(alist, tlist) {
  default_alist <- eval(formals(arx_args_list)$levels)
  default_tlist <- eval(formals(quantile_reg)$tau)
  if (setequal(alist, default_alist)) {
    if (setequal(tlist, default_tlist)) return(sort(unique(union(alist, tlist))))
    else return(sort(unique(tlist)))
  } else {
    if (setequal(tlist, default_tlist)) return(sort(unique(alist)))
    else {
      if (setequal(alist, tlist)) return(sort(unique(alist)))
      rlang::abort(c(
        "You have specified different, non-default, quantiles in the trainier and `arx_args` options.",
        i = "Please only specify quantiles in one location.")
      )
    }
  }
}
