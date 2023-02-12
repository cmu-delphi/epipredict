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

  # --- validation
  validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, "arx_alist"))
    cli_stop("args_list was not created using `arx_args_list().")
  if (!is_regression(trainer))
    cli_stop("{trainer} must be a `parsnip` method of mode 'regression'.")
  lags <- arx_lags_validator(predictors, args_list$lags)

  # --- preprocessor
  r <- epi_recipe(epi_data)
  for (l in seq_along(lags)) {
    p <- predictors[l]
    r <- step_epi_lag(r, !!p, lag = lags[[l]])
  }
  r <- r %>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead) %>%
    step_epi_naomit()
  # should limit the training window here (in an open PR)
  # What to do if insufficient training data? Add issue.

  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  target_date <- args_list$target_date %||% forecast_date + args_list$ahead

  # --- postprocessor
  f <- frosting() %>% layer_predict() # %>% layer_naomit()
  if (inherits(trainer, "quantile_reg")) {
    # add all levels to the forecaster and update postprocessor
    tau <- sort(union(args_list$levels, rlang::eval_tidy(trainer$args$tau)))
    args_list$levels <- tau
    trainer$args$tau <- rlang::enquo(tau)
    f <- layer_quantile_distn(f, levels = tau) %>% layer_point_from_distn()
  } else {
    f <- layer_residual_quantiles(
      f, probs = args_list$levels, symmetrize = args_list$symmetrize)
  }
  f <- layer_add_forecast_date(f, forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)
  if (args_list$nonneg) f <- layer_threshold(f, dplyr::starts_with(".pred"))

  # --- create test data, fit, and return
  latest <- get_test_data(r, epi_data, TRUE)
  wf <- epi_workflow(r, trainer, f) %>% generics::fit(epi_data)
  list(
    predictions = predict(wf, new_data = latest),
    epi_workflow = wf
  )
}


arx_lags_validator <- function(predictors, lags) {
  p <- length(predictors)
  if (!is.list(lags)) lags <- list(lags)
  if (length(lags) == 1) lags <- rep(lags, p)
  else if (length(lags) < p) {
    cli_stop(
      "You have requested {p} predictors but lags cannot be recycled to match."
    )
  }
  lags
}

#' ARX forecaster argument constructor
#'
#' Constructs a list of arguments for [arx_forecaster()].
#'
#' @param lags Vector or List. Positive integers enumerating lags to use
#'   in autoregressive-type models (in days).
#' @param ahead Integer. Number of time steps ahead (in days) of the forecast
#'   date for which forecasts should be produced.
#' @param min_train_window Integer. The minimal amount of training
#'   data (in the time unit of the `epi_df`) needed to produce a forecast.
#'   If smaller, the forecaster will return `NA` predictions.
#' @param forecast_date Date. The date on which the forecast is created.
#'   The default `NULL` will attempt to determine this automatically.
#' @param target_date Date. The date for which the forecast is intended.
#'   The default `NULL` will attempt to determine this automatically.
#' @param levels Vector or `NULL`. A vector of probabilities to produce
#'   prediction intervals. These are created by computing the quantiles of
#'   training residuals. A `NULL` value will result in point forecasts only.
#' @param symmetrize Logical. The default `TRUE` calculates
#'      symmetric prediction intervals.
#' @param nonneg Logical. The default `TRUE` enforces nonnegative predictions
#'   by hard-thresholding at 0.
#' @param quantile_by_key Character vector. Groups residuals by listed keys
#'   before calculating residual quantiles. See the `by_key` argument to
#'   [layer_residual_quantiles()] for more information. The default,
#'   `character(0)` performs no grouping.
#'
#' @return A list containing updated parameter choices with class `arx_alist`.
#' @export
#'
#' @examples
#' arx_args_list()
#' arx_args_list(symmetrize = FALSE)
#' arx_args_list(levels = c(.1, .3, .7, .9), min_train_window = 120)
arx_args_list <- function(lags = c(0L, 7L, 14L),
                          ahead = 7L,
                          min_train_window = 20L,
                          forecast_date = NULL,
                          target_date = NULL,
                          levels = c(0.05, 0.95),
                          symmetrize = TRUE,
                          nonneg = TRUE,
                          quantile_by_key = character(0L)) {

  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  arg_is_scalar(ahead, min_train_window, symmetrize, nonneg)
  arg_is_chr(quantile_by_key, allow_null = TRUE)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_date(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, min_train_window, lags)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(levels, allow_null = TRUE)

  max_lags <- max(lags)
  structure(enlist(lags = .lags,
                   ahead,
                   min_train_window,
                   levels,
                   forecast_date,
                   target_date,
                   symmetrize,
                   nonneg,
                   max_lags,
                   quantile_by_key),
            class = "arx_alist")
}
