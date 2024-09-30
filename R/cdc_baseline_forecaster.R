#' Predict the future with the most recent value
#'
#' This is a simple forecasting model for
#' [epiprocess::epi_df][epiprocess::as_epi_df] data. It uses the most recent observation as the
#' forecast for any future date, and produces intervals by shuffling the quantiles
#' of the residuals of such a "flatline" forecast and incrementing these
#' forward over all available training data.
#'
#' By default, the predictive intervals are computed separately for each
#' combination of `geo_value` in the `epi_data` argument.
#'
#' This forecaster is meant to produce exactly the CDC Baseline used for
#' [COVID19ForecastHub](https://covid19forecasthub.org)
#'
#' @param epi_data An [`epiprocess::epi_df`][epiprocess::as_epi_df]
#' @param outcome A scalar character for the column name we wish to predict.
#' @param args_list A list of additional arguments as created by the
#'   [cdc_baseline_args_list()] constructor function.
#'
#' @return A data frame of point and interval forecasts for all aheads (unique
#'   horizons) for each unique combination of `key_vars`.
#' @export
#'
#' @examples
#' library(dplyr)
#' weekly_deaths <- case_death_rate_subset %>%
#'   select(geo_value, time_value, death_rate) %>%
#'   left_join(state_census %>% select(pop, abbr), by = c("geo_value" = "abbr")) %>%
#'   mutate(deaths = pmax(death_rate / 1e5 * pop * 7, 0)) %>%
#'   select(-pop, -death_rate) %>%
#'   group_by(geo_value) %>%
#'   epi_slide(~ sum(.$deaths), .window_size = 7, .new_col_name = "deaths_7dsum") %>%
#'   ungroup() %>%
#'   filter(weekdays(time_value) == "Saturday")
#'
#' cdc <- cdc_baseline_forecaster(weekly_deaths, "deaths_7dsum")
#' preds <- pivot_quantiles_wider(cdc$predictions, .pred_distn)
#'
#' if (require(ggplot2)) {
#'   forecast_date <- unique(preds$forecast_date)
#'   four_states <- c("ca", "pa", "wa", "ny")
#'   preds %>%
#'     filter(geo_value %in% four_states) %>%
#'     ggplot(aes(target_date)) +
#'     geom_ribbon(aes(ymin = `0.1`, ymax = `0.9`), fill = blues9[3]) +
#'     geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), fill = blues9[6]) +
#'     geom_line(aes(y = .pred), color = "orange") +
#'     geom_line(
#'       data = weekly_deaths %>% filter(geo_value %in% four_states),
#'       aes(x = time_value, y = deaths_7dsum)
#'     ) +
#'     scale_x_date(limits = c(forecast_date - 90, forecast_date + 30)) +
#'     labs(x = "Date", y = "Weekly deaths") +
#'     facet_wrap(~geo_value, scales = "free_y") +
#'     theme_bw() +
#'     geom_vline(xintercept = forecast_date)
#' }
cdc_baseline_forecaster <- function(
    epi_data,
    outcome,
    args_list = cdc_baseline_args_list()) {
  validate_forecaster_inputs(epi_data, outcome, "time_value")
  if (!inherits(args_list, c("cdc_flat_fcast", "alist"))) {
    cli_abort("`args_list` was not created using `cdc_baseline_args_list().")
  }
  keys <- key_colnames(epi_data)
  ek <- kill_time_value(keys)
  outcome <- rlang::sym(outcome)


  r <- epi_recipe(epi_data) %>%
    step_epi_ahead(!!outcome, ahead = args_list$data_frequency, skip = TRUE) %>%
    recipes::update_role(!!outcome, new_role = "predictor") %>%
    recipes::add_role(tidyselect::all_of(keys), new_role = "predictor") %>%
    step_training_window(n_recent = args_list$n_training)

  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  # target_date <- args_list$target_date %||% (forecast_date + args_list$ahead)


  latest <- get_test_data(
    epi_recipe(epi_data), epi_data, TRUE, args_list$nafill_buffer,
    forecast_date
  )

  f <- frosting() %>%
    layer_predict() %>%
    layer_cdc_flatline_quantiles(
      aheads = args_list$aheads,
      quantile_levels = args_list$quantile_levels,
      nsims = args_list$nsims,
      by_key = args_list$quantile_by_key,
      symmetrize = args_list$symmetrize,
      nonneg = args_list$nonneg
    ) %>%
    layer_add_forecast_date(forecast_date = forecast_date) %>%
    layer_unnest(.pred_distn_all)
  # layer_add_target_date(target_date = target_date)
  if (args_list$nonneg) f <- layer_threshold(f, ".pred")

  eng <- linear_reg(engine = "flatline")

  wf <- epi_workflow(r, eng, f)
  wf <- fit(wf, epi_data)
  preds <- suppressWarnings(predict(wf, new_data = latest)) %>%
    as_tibble() %>%
    select(-time_value) %>%
    mutate(target_date = forecast_date + ahead * args_list$data_frequency)

  structure(
    list(
      predictions = preds,
      epi_workflow = wf,
      metadata = list(
        training = attr(epi_data, "metadata"),
        forecast_created = Sys.time()
      )
    ),
    class = c("cdc_baseline_fcast", "canned_epipred")
  )
}



#' CDC baseline forecaster argument constructor
#'
#' Constructs a list of arguments for [cdc_baseline_forecaster()].
#'
#' @inheritParams arx_args_list
#' @param data_frequency Integer or string. This describes the frequency of the
#'   input `epi_df`. For typical FluSight forecasts, this would be `"1 week"`.
#'   Allowable arguments are integers (taken to mean numbers of days) or a
#'   string like `"7 days"` or `"2 weeks"`. Currently, all other periods
#'   (other than days or weeks) result in an error.
#' @param aheads Integer vector. Unlike [arx_forecaster()], this doesn't have
#'   any effect on the predicted values.
#'   Predictions are always the most recent observation. This determines the
#'   set of prediction horizons for [layer_cdc_flatline_quantiles()]`. It interacts
#'   with the `data_frequency` argument. So, for example, if the data is daily
#'   and you want forecasts for 1:4 days ahead, then you would use `1:4`. However,
#'   if you want one-week predictions, you would set this as `c(7, 14, 21, 28)`.
#'   But if `data_frequency` is `"1 week"`, then you would set it as `1:4`.
#' @param quantile_levels Vector or `NULL`. A vector of probabilities to produce
#'   prediction intervals. These are created by computing the quantiles of
#'   training residuals. A `NULL` value will result in point forecasts only.
#' @param nsims Positive integer. The number of draws from the empirical CDF.
#'   These samples are spaced evenly on the (0, 1) scale, F_X(x) resulting
#'   in linear interpolation on the X scale. This is achieved with
#'   [stats::quantile()] Type 7 (the default for that function).
#' @param nonneg Logical. Force all predictive intervals be non-negative.
#'   Because non-negativity is forced _before_ propagating forward, this
#'   has slightly different behaviour than would occur if using
#'   [layer_threshold()].
#'
#' @return A list containing updated parameter choices with class `cdc_flat_fcast`.
#' @export
#'
#' @examples
#' cdc_baseline_args_list()
#' cdc_baseline_args_list(symmetrize = FALSE)
#' cdc_baseline_args_list(quantile_levels = c(.1, .3, .7, .9), n_training = 120)
cdc_baseline_args_list <- function(
    data_frequency = "1 week",
    aheads = 1:5,
    n_training = Inf,
    forecast_date = NULL,
    quantile_levels = c(.01, .025, 1:19 / 20, .975, .99),
    nsims = 1e5L,
    symmetrize = TRUE,
    nonneg = TRUE,
    quantile_by_key = "geo_value",
    nafill_buffer = Inf,
    ...) {
  rlang::check_dots_empty()
  arg_is_scalar(n_training, nsims, data_frequency)
  data_frequency <- parse_period(data_frequency)
  arg_is_pos_int(data_frequency)
  arg_is_chr(quantile_by_key, allow_empty = TRUE)
  arg_is_scalar(forecast_date, allow_null = TRUE)
  arg_is_date(forecast_date, allow_null = TRUE)
  arg_is_nonneg_int(aheads, nsims)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(quantile_levels, allow_null = TRUE)
  arg_is_pos(n_training)
  if (is.finite(n_training)) arg_is_pos_int(n_training)
  if (is.finite(nafill_buffer)) arg_is_pos_int(nafill_buffer, allow_null = TRUE)

  structure(
    enlist(
      data_frequency,
      aheads,
      n_training,
      forecast_date,
      quantile_levels,
      nsims,
      symmetrize,
      nonneg,
      quantile_by_key,
      nafill_buffer
    ),
    class = c("cdc_baseline_fcast", "alist")
  )
}

#' @export
print.cdc_baseline_fcast <- function(x, ...) {
  name <- "CDC Baseline"
  NextMethod(name = name, ...)
}

parse_period <- function(x) {
  arg_is_scalar(x)
  if (is.character(x)) {
    x <- unlist(strsplit(x, " "))
    if (length(x) == 1L) x <- as.numeric(x)
    if (length(x) == 2L) {
      mult <- substr(x[2], 1, 3)
      mult <- switch(mult,
        day = 1L,
        wee = 7L,
        cli_abort("incompatible timespan in `aheads`.")
      )
      x <- as.numeric(x[1]) * mult
    }
    if (length(x) > 2L) cli_abort("incompatible timespan in `aheads`.")
  }
  stopifnot(rlang::is_integerish(x))
  as.integer(x)
}
