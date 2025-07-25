#' Predict the future with today's value
#'
#' @description This is a simple forecasting model for
#'   [epiprocess::epi_df][epiprocess::as_epi_df] data. It uses the most recent
#'   observation as the forecast for any future date, and produces intervals
#'   based on the quantiles of the residuals of such a "flatline" forecast over
#'   all available training data.
#'
#' By default, the predictive intervals are computed separately for each
#'   combination of key values (`geo_value` + any additional keys) in the
#'   `epi_data` argument.
#'
#' This forecaster is very similar to that used by the
#'   [COVID19ForecastHub](https://covid19forecasthub.org)
#'
#' @details
#'  Here is (roughly) the code for the `flatline_forecaster()` applied to the
#'   `case_rate` for `epidatasets::covid_case_death_rates`.
#'
#' ```{r}
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_epi_ahead(case_rate, ahead = 7, skip = TRUE) %>%
#'   recipes::update_role(case_rate, new_role = "predictor") %>%
#'   recipes::add_role(all_of(key_colnames(jhu)), new_role = "predictor")
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles() %>%
#'   layer_add_forecast_date() %>%
#'   layer_add_target_date() %>%
#'   layer_threshold(starts_with(".pred"))
#'
#' eng <- linear_reg() %>% set_engine("flatline")
#' wf <- epi_workflow(r, eng, f) %>% fit(jhu)
#' preds <- forecast(wf)
#' ```
#'
#' @param epi_data An [epiprocess::epi_df][epiprocess::as_epi_df]
#' @param outcome A scalar character for the column name we wish to predict.
#' @param args_list A list of additional arguments as created by the
#'   [flatline_args_list()] constructor function.
#'
#' @return A data frame of point (and optionally interval) forecasts at a single
#'   ahead (unique horizon) for each unique combination of `key_vars`.
#' @export
#'
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   dplyr::filter(time_value >= as.Date("2021-12-01"))
#'
#' out <- flatline_forecaster(jhu, "death_rate")
flatline_forecaster <- function(
    epi_data,
    outcome,
    args_list = flatline_args_list()) {
  validate_forecaster_inputs(epi_data, outcome, "time_value")
  if (!inherits(args_list, c("flat_fcast", "alist"))) {
    cli_abort("`args_list` was not created using `flatline_args_list()`.")
  }
  keys <- key_colnames(epi_data)
  ek <- kill_time_value(keys)
  outcome <- rlang::sym(outcome)


  r <- epi_recipe(epi_data) %>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead, skip = TRUE) %>%
    recipes::update_role(!!outcome, new_role = "predictor") %>%
    recipes::add_role(tidyselect::all_of(keys), new_role = "predictor") %>%
    step_training_window(n_recent = args_list$n_training)

  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  target_date <- args_list$target_date %||% (forecast_date + args_list$ahead)

  f <- frosting() %>%
    layer_predict() %>%
    layer_residual_quantiles(
      quantile_levels = args_list$quantile_levels,
      symmetrize = args_list$symmetrize,
      by_key = args_list$quantile_by_key
    ) %>%
    layer_add_forecast_date(forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)
  if (args_list$nonneg) f <- layer_threshold(f, dplyr::starts_with(".pred"))

  eng <- linear_reg(engine = "flatline")

  wf <- epi_workflow(r, eng, f) %>%
    fit(epi_data)
  preds <- suppressWarnings(forecast(wf)) %>%
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
    class = c("flat_fcast", "canned_epipred")
  )
}



#' Flatline forecaster argument constructor
#'
#' Constructs a list of arguments for [flatline_forecaster()].
#'
#' @inheritParams arx_args_list
#' @param ahead Integer. Unlike [arx_forecaster()], this doesn't have any effect
#'   on the predicted values. Predictions are always the most recent observation.
#'   However, this _does_ impact the residuals stored in the object. Residuals
#'   are calculated based on this number to mimic how badly you would have done.
#'   So for example, `ahead = 7` will create residuals by comparing values
#'   7 days apart.
#'
#' @return A list containing updated parameter choices with class `flatline_alist`.
#' @export
#'
#' @examples
#' flatline_args_list()
#' flatline_args_list(symmetrize = FALSE)
#' flatline_args_list(quantile_levels = c(.1, .3, .7, .9), n_training = 120)
flatline_args_list <- function(
    ahead = 7L,
    n_training = Inf,
    forecast_date = NULL,
    target_date = NULL,
    quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
    symmetrize = TRUE,
    nonneg = TRUE,
    quantile_by_key = character(0L),
    ...) {
  rlang::check_dots_empty()
  arg_is_scalar(ahead, n_training)
  arg_is_chr(quantile_by_key, allow_empty = TRUE)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_date(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(quantile_levels, allow_null = TRUE)
  arg_is_pos(n_training)
  if (is.finite(n_training)) arg_is_pos_int(n_training)

  if (!is.null(forecast_date) && !is.null(target_date)) {
    if (forecast_date + ahead != target_date) {
      cli_warn(c(
        "`forecast_date` + `ahead` must equal `target_date`.",
        i = "{.val {forecast_date}} + {.val {ahead}} != {.val {target_date}}."
      ))
    }
  }

  structure(
    enlist(
      ahead,
      n_training,
      forecast_date,
      target_date,
      quantile_levels,
      symmetrize,
      nonneg,
      quantile_by_key
    ),
    class = c("flat_fcast", "alist")
  )
}

#' @export
print.flat_fcast <- function(x, ...) {
  name <- "flatline"
  NextMethod(name = name, ...)
}
