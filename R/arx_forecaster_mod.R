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
#' out <- arx_epi_forecaster(jhu, "death_rate",
#'   c("case_rate", "death_rate"))
arx_epi_forecaster <- function(epi_data,
                               outcome,
                               predictors,
                               trainer = parsnip::linear_reg(),
                               args_list = arx_args_list()) {

  validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!is.list(trainer) || trainer$mode != "regression")
    cli_stop("{trainer} must be a `parsnip` method of mode 'regression'.")

  lags <- arx_lags_validator(predictors, args_list$lags)

  ## Recipe
  r <- epi_recipe(epi_data)
  for (l in seq_along(lags)) {
    p <- predictors[l]
    r <- step_epi_lag(r, !!p, lag = lags[[l]])
  }
  r <- r %>%
    step_epi_ahead(dplyr::all_of(!!outcome), ahead = args_list$ahead) %>%
    step_epi_naomit()
  # should limit the training window here (in an open PR)
  # What to do if insufficient training data? Add issue.

  fd <- max(epi_data$time_value)
  td <- fd + args_list$ahead
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(
      probs = args_list$levels,
      symmetrize = args_list$symmetrize) %>%
    layer_add_forecast_date(forecast_date = fd) %>%
    layer_add_target_date(target_date = td)
  if (args_list$nonneg) f <- layer_threshold(f, dplyr::starts_with(".pred"))
  # need the target date processing here

  latest <- get_test_data(r, epi_data)

  wf <- epi_workflow(r, trainer) %>% # bug, issue 72
    add_frosting(f) %>%
    fit(epi_data)
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
