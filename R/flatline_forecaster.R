#' Predict the future with today's value
#'
#' This is a simple forecasting model for
#' [epiprocess::epi_df] data. It uses the most recent observation as the
#' forcast for any future date, and produces intervals based on the quantiles
#' of the residuals of such a "flatline" forecast over all available training
#' data.
#'
#' By default, the predictive intervals are computed separately for each
#' combination of key values (`geo_value` + any additional keys) in the
#' `epi_data` argument.
#'
#' This forecaster is very similar to that used by the
#' [COVID19ForecastHub](https://covid19forecasthub.org)
#'
#' @param epi_data An [epiprocess::epi_df]
#' @param outcome A scalar character for the column name we wish to predict.
#' @param args_list A list of dditional arguments as created by the
#'   [flatline_args_list()] constructor function.
#'
#' @return A data frame of point (and optionally interval) forecasts at a single
#'   ahead (unique horizon) for each unique combination of `key_vars`.
#' @export
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value >= as.Date("2021-12-01"))
#'
#' out <- flatline_forecaster(jhu, "death_rate")
flatline_forecaster <- function(
    epi_data,
    outcome,
    args_list = flatline_args_list()) {
  validate_forecaster_inputs(epi_data, outcome, "time_value")
  if (!inherits(args_list, c("flat_fcast", "alist"))) {
    cli_stop("args_list was not created using `flatline_args_list().")
  }
  keys <- epi_keys(epi_data)
  ek <- kill_time_value(keys)
  outcome <- rlang::sym(outcome)


  r <- epi_recipe(epi_data) %>%
    step_epi_ahead(!!outcome, ahead = args_list$ahead, skip = TRUE) %>%
    recipes::update_role(!!outcome, new_role = "predictor") %>%
    recipes::add_role(tidyselect::all_of(keys), new_role = "predictor") %>%
    step_training_window(n_recent = args_list$n_training)

  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  target_date <- args_list$target_date %||% (forecast_date + args_list$ahead)

  latest <- get_test_data(
    epi_recipe(epi_data), epi_data, TRUE, args_list$nafill_buffer,
    forecast_date
  )

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

  eng <- parsnip::linear_reg() %>% parsnip::set_engine("flatline")

  wf <- epi_workflow(r, eng, f)
  wf <- generics::fit(wf, epi_data)
  preds <- suppressWarnings(predict(wf, new_data = latest)) %>%
    tibble::as_tibble() %>%
    dplyr::select(-time_value)

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
    quantile_levels = c(0.05, 0.95),
    symmetrize = TRUE,
    nonneg = TRUE,
    quantile_by_key = character(0L),
    nafill_buffer = Inf,
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
  if (is.finite(nafill_buffer)) arg_is_pos_int(nafill_buffer, allow_null = TRUE)

  if (!is.null(forecast_date) && !is.null(target_date)) {
    if (forecast_date + ahead != target_date) {
      cli::cli_warn(c(
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
      quantile_by_key,
      nafill_buffer
    ),
    class = c("flat_fcast", "alist")
  )
}

#' @export
print.flat_fcast <- function(x, ...) {
  name <- "flatline"
  NextMethod(name = name, ...)
}
