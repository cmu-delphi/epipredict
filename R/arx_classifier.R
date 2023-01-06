#' Direct autoregressive classifier with covariates
#'
#' This is an autoregressive classification model for
#' [epiprocess::epi_df] data. It does "direct" forecasting, meaning
#' that it estimates a class at a particular target horizon.
#'
#' @inheritParams arx_forecaster
#' @param trainer A `{parsnip}` model describing the type of estimation.
#'   For now, we enforce `mode = "classification"`. Typical values are
#'   [parsnip::logistic_reg()] or [parsnip::multinom_reg()]. More complicated
#'   trainers like [parsnip::naive_bayes()] or [parsnip::rand_forest()] can
#'   also be used.
#' @param args_list A list of customization arguments to determine
#'   the type of forecasting model. See [arx_args_list()].
#'
#' @return A list with (1) `predictions` an `epi_df` of predicted classes
#'   and (2) `epi_workflow`, a list that encapsulates the entire estimation
#'   workflow
#' @export
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value >= as.Date("2021-12-01"))
#'
#' out <- arx_classifier(jhu, "death_rate",
#'   c("case_rate", "death_rate"))
#'
#' out <- arx_forecaster(jhu, "death_rate",
#'   c("case_rate", "death_rate"), trainer = parsnip::multinom_reg(),
#'   args_list = arx_class_args_list(
#'     breaks = c(-.2, .25), ahead = 14, horizon = 14, method = "linear_reg"))
arx_classifier <- function(epi_data,
                           outcome,
                           predictors,
                           trainer = parsnip::logistic_reg(),
                           args_list = arx_class_args_list()) {

  # --- validation
  validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, "arx_clist"))
    cli_stop("args_list was not created using `arx_class_args_list().")
  if (!is_classification(trainer))
    cli_stop("{trainer} must be a `parsnip` method of mode 'classification'.")
  lags <- arx_lags_validator(predictors, args_list$lags)

  # --- preprocessor
  r <- epi_recipe(epi_data) %>%
    step_growth_rate(
      union(outcome, predictors), role = "gr",
      horizon = args_list$horizon, method = args_list$method,
      log_scale = args_list$log_scale,
      additional_gr_args_list = args_list$additional_gr_args)
  for (l in seq_along(lags)) {
    p <- predictors[l]
    p <- glue::glue("gr_{args_list$horizon}_{args_list$method}_{p}")
    r <- step_epi_lag(r, !!p, lag = lags[[l]])
  }
  o <- glue::glue("gr_{args_list$horizon}_{args_list$method}_{outcome}")
  r <- step_epi_ahead(r, dplyr::all_of(!!o), ahead = args_list$ahead) %>%
    step_cut(all_outcomes(),
             breaks = args_list$breaks,
             include_outside_range = TRUE) %>%
    step_epi_naomit()

  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  target_date <- args_list$target_date %||% forecast_date + args_list$ahead

  # --- postprocessor
  f <- frosting() %>% layer_predict() # %>% layer_naomit()
  f <- layer_add_forecast_date(f, forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)


  # --- create test data, fit, and return
  latest <- get_test_data(r, epi_data, TRUE)
  wf <- epi_workflow(r, trainer, f) %>% generics::fit(epi_data)
  list(
    predictions = predict(wf, new_data = latest),
    epi_workflow = wf
  )
}



#' ARX classifier argument constructor
#'
#' Constructs a list of arguments for [arx_classifier()].
#'
#' @inheritParams arx_args_list
#' @param breaks Vector. A vector of breaks to turn real-valued growth rates
#'   into discrete classes. The default gives binary upswing classification
#'   as in [McDonald, Bien, Green, Hu, et al.](https://doi.org/10.1073/pnas.2111453118).
#'   This coincides with the default `trainer = parsnip::logistic_reg()`
#'   argument in [arx_classifier()]. However, multiclass classification is also
#'   supported (e.g. with `breaks = c(-.2, .25)`) provided that
#'   `trainer = parsnip::multinom_reg()` (or another multiclass predictor)
#'   is used as well.
#' @param horizon Scalar integer. This is passed to the `h` argument of
#'   [epiprocess::growth_rate()]. It determines the amount of data used to
#'   calculate the growth rate.
#' @param method Character. Options available for growth rate calculation.
#' @param log_scale Scalar logical. Whether to compute growth rates on the
#'   log scale.
#' @param additional_gr_args List. Optional arguments controlling growth rate
#'   calculation. See [epiprocess::growth_rate()] and the related Vignette for
#'   more details.
#'
#' @return A list containing updated parameter choices with class `arx_alist`.
#' @export
#'
#' @examples
#' arx_class_args_list()
#'
#' # 3-class classsification,
#' # also needs arx_classifier(trainer = parsnip::multinom_reg())
#' arx_class_args_list(breaks = c(-.2, .25))
arx_class_args_list <- function(
    lags = c(0L, 7L, 14L),
    ahead = 7L,
    min_train_window = 20L,
    forecast_date = NULL,
    target_date = NULL,
    breaks = 0.25,
    horizon = 7L,
    method = c("rel_change", "linear_reg", "smooth_spline", "trend_filter"),
    log_scale = FALSE,
    additional_gr_args = list()
) {

  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)
  method <- match.arg(method)

  arg_is_scalar(ahead, min_train_window, horizon, log_scale)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_date(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, min_train_window, lags, horizon)
  arg_is_numeric(breaks)
  arg_is_lgl(log_scale)
  if (!is.list(additional_gr_args)) {
    rlang::abort(
      c("`additional_gr_args` must be a list.",
        i = "See `?epiprocess::growth_rate` for available arguments.")
    )
  }

  max_lags <- max(lags)
  structure(
    enlist(lags = .lags,
           ahead,
           min_train_window,
           breaks,
           forecast_date,
           target_date,
           max_lags,
           horizon,
           method,
           log_scale,
           additional_gr_args
    ),
    class = "arx_clist"
  )
}
