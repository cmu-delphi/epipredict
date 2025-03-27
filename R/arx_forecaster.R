#' Direct autoregressive forecaster with covariates
#'
#' This is an autoregressive forecasting model for
#' [epiprocess::epi_df][epiprocess::as_epi_df] data. It does "direct" forecasting, meaning
#' that it estimates a model for a particular target horizon.
#'
#'
#' @param epi_data An `epi_df` object
#' @param outcome A character (scalar) specifying the outcome (in the
#'   `epi_df`).
#' @param predictors A character vector giving column(s) of predictor variables.
#'   This defaults to the `outcome`. However, if manually specified, only those variables
#'   specifically mentioned will be used. (The `outcome` will not be added.)
#'   By default, equals the outcome. If manually specified, does not add the
#'   outcome variable, so make sure to specify it.
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
#' jhu <- covid_case_death_rates %>%
#'   dplyr::filter(time_value >= as.Date("2021-12-01"))
#'
#' out <- arx_forecaster(
#'   jhu, "death_rate",
#'   c("case_rate", "death_rate")
#' )
#'
#' out <- arx_forecaster(jhu, "death_rate",
#'   c("case_rate", "death_rate"),
#'   trainer = quantile_reg(),
#'   args_list = arx_args_list(quantile_levels = 1:9 / 10)
#' )
arx_forecaster <- function(
    epi_data,
    outcome,
    predictors = outcome,
    trainer = linear_reg(),
    args_list = arx_args_list()) {
  if (!is_regression(trainer)) {
    cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'regression'.")
  }

  wf <- arx_fcast_epi_workflow(epi_data, outcome, predictors, trainer, args_list)
  wf <- fit(wf, epi_data)

  # get the forecast date for the forecast function
  if (args_list$adjust_latency == "none") {
    forecast_date_default <- max(epi_data$time_value)
  } else {
    forecast_date_default <- attributes(epi_data)$metadata$as_of
  }
  forecast_date <- args_list$forecast_date %||% forecast_date_default


  preds <- forecast(wf, forecast_date = forecast_date) %>%
    as_tibble() %>%
    select(-time_value)

  structure(
    list(
      predictions = preds,
      epi_workflow = wf,
      metadata = list(
        training = attr(epi_data, "metadata"),
        forecast_created = Sys.time()
      )
    ),
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
#' @param trainer A `{parsnip}` model describing the type of estimation. For
#'   now, we enforce `mode = "regression"`. May be `NULL` if you'd like to
#'   decide later.
#'
#' @return An unfitted `epi_workflow`.
#' @export
#' @seealso [arx_forecaster()], [arx_args_list()]
#'
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value >= as.Date("2021-12-01"))
#'
#' arx_fcast_epi_workflow(
#'   jhu, "death_rate",
#'   c("case_rate", "death_rate")
#' )
#'
#' arx_fcast_epi_workflow(jhu, "death_rate",
#'   c("case_rate", "death_rate"),
#'   trainer = quantile_reg(),
#'   args_list = arx_args_list(quantile_levels = 1:9 / 10)
#' )
arx_fcast_epi_workflow <- function(
    epi_data,
    outcome,
    predictors = outcome,
    trainer = linear_reg(),
    args_list = arx_args_list()) {
  # --- validation
  validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_fcast", "alist"))) {
    cli_abort("`args_list` was not created using `arx_args_list()`.")
  }
  if (!(is.null(trainer) || is_regression(trainer))) {
    cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'regression'.")
  }
  # forecast_date is above all what they set;
  # if they don't and they're not adjusting latency, it defaults to the max time_value
  # if they're adjusting, it defaults to the as_of
  if (args_list$adjust_latency == "none") {
    forecast_date_default <- max(epi_data$time_value)
    if (!is.null(args_list$forecast_date) && args_list$forecast_date != forecast_date_default) {
      cli_warn(
        "The specified forecast date {args_list$forecast_date} doesn't match the date from which the forecast is actually occurring {forecast_date_default}.",
        class = "epipredict__arx_forecaster__forecast_date_defaulting"
      )
    }
  } else {
    forecast_date_default <- attributes(epi_data)$metadata$as_of
  }
  forecast_date <- args_list$forecast_date %||% forecast_date_default
  target_date <- args_list$target_date %||% (forecast_date + args_list$ahead)
  if (forecast_date + args_list$ahead != target_date) {
    cli_abort("`forecast_date` {.val {forecast_date}} + `ahead` {.val {ahead}} must equal `target_date` {.val {target_date}}.",
      class = "epipredict__arx_forecaster__inconsistent_target_ahead_forecaste_date"
    )
  }

  lags <- arx_lags_validator(predictors, args_list$lags)

  # --- preprocessor
  r <- epi_recipe(epi_data)
  # adjust latency if the user asks
  method_adjust_latency <- args_list$adjust_latency
  if (!is.null(method_adjust_latency)) {
    if (method_adjust_latency == "extend_ahead") {
      r <- r %>% step_adjust_latency(all_outcomes(),
        fixed_forecast_date = forecast_date,
        method = method_adjust_latency
      )
    } else if (method_adjust_latency == "extend_lags") {
      r <- r %>% step_adjust_latency(all_predictors(),
        fixed_forecast_date = forecast_date,
        method = method_adjust_latency
      )
    }
  }
  for (l in seq_along(lags)) {
    p <- predictors[l]
    r <- step_epi_lag(r, !!p, lag = lags[[l]])
  }
  r <- r %>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead)
  r <- r %>%
    step_epi_naomit() %>%
    step_training_window(n_recent = args_list$n_training) %>%
    check_enough_data(all_predictors(), min_data_points = 1, skip = FALSE)

  if (!is.null(args_list$check_enough_data_n)) {
    r <- r %>% check_enough_data(
      all_predictors(),
      all_outcomes(),
      min_data_points = args_list$check_enough_data_n,
      epi_keys = args_list$check_enough_data_epi_keys,
      drop_na = FALSE
    )
  }


  # --- postprocessor
  f <- frosting() %>% layer_predict() # %>% layer_naomit()
  is_quantile_reg <- inherits(trainer, "quantile_reg") |
    (inherits(trainer, "rand_forest") & trainer$engine == "grf_quantiles")
  if (is_quantile_reg) {
    # add all quantile_level to the forecaster and update postprocessor
    if (inherits(trainer, "quantile_reg")) {
      quantile_levels <- sort(compare_quantile_args(
        args_list$quantile_levels,
        rlang::eval_tidy(trainer$args$quantile_levels),
        "qr"
      ))
      trainer$args$quantile_levels <- rlang::enquo(quantile_levels)
    } else {
      quantile_levels <- sort(compare_quantile_args(
        args_list$quantile_levels,
        rlang::eval_tidy(trainer$eng_args$quantiles) %||%
          c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
        "grf"
      ))
      trainer$eng_args$quantiles <- rlang::enquo(quantile_levels)
    }
    args_list$quantile_levels <- quantile_levels
    f <- f %>%
      layer_quantile_distn(quantile_levels = quantile_levels) %>%
      layer_point_from_distn()
  } else {
    f <- f %>% layer_residual_quantiles(
      quantile_levels = args_list$quantile_levels,
      symmetrize = args_list$symmetrize,
      by_key = args_list$quantile_by_key
    )
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
#' @param forecast_date Date. The date from which the forecast is occurring.
#'   The default `NULL` will determine this automatically from either
#'   1. the maximum time value for which there's data if there is no latency
#'   adjustment (the default case), or
#'   2. the `as_of` date of `epi_data` if `adjust_latency` is
#'   non-`NULL`.
#' @param target_date Date. The date that is being forecast. The default `NULL`
#'   will determine this automatically as `forecast_date + ahead`.
#' @param adjust_latency Character. One of the `method`s of
#'   [step_adjust_latency()], or `"none"` (in which case there is no adjustment).
#'   If the `forecast_date` is after the last day of data, this determines how
#'   to shift the model to account for this difference. The options are:
#'   - `"none"` the default, assumes the `forecast_date` is the last day of data
#'   - `"extend_ahead"`: increase the `ahead` by the latency so it's relative to
#'   the last day of data. For example, if the last day of data was 3 days ago,
#'   the ahead becomes `ahead+3`.
#'   - `"extend_lags"`: increase the lags so they're relative to the actual
#'   forecast date. For example, if the lags are `c(0, 7, 14)` and the last day of
#'   data was 3 days ago, the lags become `c(3, 10, 17)`.
#' @param warn_latency by default, `step_adjust_latency` warns the user if the
#'   latency is large. If this is `FALSE`, that warning is turned off.
#' @param quantile_levels Vector or `NULL`. A vector of probabilities to produce
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
#' @param check_enough_data_n Integer. A lower limit for the number of rows per
#'   epi_key that are required for training. If `NULL`, this check is ignored.
#' @param check_enough_data_epi_keys Character vector. A character vector of
#'   column names on which to group the data and check threshold within each
#'   group. Useful if training per group (for example, per geo_value).
#' @param ... Space to handle future expansions (unused).
#'
#'
#' @return A list containing updated parameter choices with class `arx_flist`.
#' @export
#' @seealso [arx_forecaster()]
#'
#' @examples
#' arx_args_list()
#' arx_args_list(symmetrize = FALSE)
#' arx_args_list(quantile_levels = c(.1, .3, .7, .9), n_training = 120)
arx_args_list <- function(
    lags = c(0L, 7L, 14L),
    ahead = 7L,
    n_training = Inf,
    forecast_date = NULL,
    target_date = NULL,
    adjust_latency = c("none", "extend_ahead", "extend_lags", "locf"),
    warn_latency = TRUE,
    quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
    symmetrize = TRUE,
    nonneg = TRUE,
    quantile_by_key = character(0L),
    check_enough_data_n = NULL,
    check_enough_data_epi_keys = NULL,
    ...) {
  # error checking if lags is a list
  rlang::check_dots_empty()
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  adjust_latency <- rlang::arg_match(adjust_latency)
  arg_is_scalar(ahead, n_training, symmetrize, nonneg, adjust_latency, warn_latency)
  arg_is_chr(quantile_by_key, allow_empty = TRUE)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_date(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, lags)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(quantile_levels, allow_null = TRUE)
  arg_is_pos(n_training)
  if (is.finite(n_training)) arg_is_pos_int(n_training)
  arg_is_pos(check_enough_data_n, allow_null = TRUE)
  arg_is_chr(check_enough_data_epi_keys, allow_null = TRUE)

  if (!is.null(forecast_date) && !is.null(target_date)) {
    if (forecast_date + ahead != target_date) {
      cli_abort("`forecast_date` {.val {forecast_date}} + `ahead` {.val {ahead}} must equal `target_date` {.val {target_date}}.",
        class = "epipredict__arx_args__inconsistent_target_ahead_forecaste_date"
      )
    }
  }

  max_lags <- max(lags)
  structure(
    enlist(
      lags = .lags,
      ahead,
      n_training,
      quantile_levels,
      forecast_date,
      target_date,
      adjust_latency,
      warn_latency,
      symmetrize,
      nonneg,
      max_lags,
      quantile_by_key,
      check_enough_data_n,
      check_enough_data_epi_keys
    ),
    class = c("arx_fcast", "alist")
  )
}


#' @export
print.arx_fcast <- function(x, ...) {
  name <- "ARX Forecaster"
  NextMethod(name = name, ...)
}

compare_quantile_args <- function(alist, tlist, train_method = c("qr", "grf")) {
  train_method <- rlang::arg_match(train_method)
  default_alist <- eval(formals(arx_args_list)$quantile_levels)
  default_tlist <- switch(train_method,
    "qr" = eval(formals(quantile_reg)$quantile_levels),
    "grf" = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
  )
  if (setequal(alist, default_alist)) {
    if (setequal(tlist, default_tlist)) {
      return(sort(unique(union(alist, tlist))))
    } else {
      return(sort(unique(tlist)))
    }
  } else {
    if (setequal(tlist, default_tlist)) {
      return(sort(unique(alist)))
    } else {
      if (setequal(alist, tlist)) {
        return(sort(unique(alist)))
      }
      cli_abort(c(
        "You have specified different, non-default, quantiles in the trainier and `arx_args` options.",
        i = "Please only specify quantiles in one location."
      ))
    }
  }
}
