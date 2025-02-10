#' Climatological forecaster
#'
#' This is another "baseline" type forecaster, but it is especially appropriate
#' for strongly seasonal diseases (e.g., influenza). The idea is to predict
#' the "typical season" by summarizing over all available history in the
#' `epi_data`. This is analogous to a "climate" forecast rather than a "weather"
#' forecast, essentially predicting "typical January" behavior by relying on a
#' long history of such periods rather than heavily using recent data.
#'
#' The forecast is taken as the quantiles of the `outcome` in a small window
#' around the target period, computed over the entire available history.
#'
#' @inheritParams flatline_forecaster
#' @param args_list A list of additional arguments as created by the
#'   [climate_args_list()] constructor function.
#'
#' @return A data frame of point and interval) forecasts at a all horizons
#'  for each unique combination of `key_vars`.
#' @export
#' @seealso [step_climate()]
#'
#' @examples
#'
#' 1 + 1
#' 2 + 2
climatological_forecaster <- function(epi_data,
                                      outcome,
                                      args_list = climate_args_list()) {
  if (!is_epi_df(epi_data)) {
    cli_abort(
      "`epi_data` must be an {.cls epi_df}, not a {.cls {class(epi_data)}}."
    )
  }
  edf_time_type <- attr(epi_data, "metadata")$time_type
  if (edf_time_type == "custom") {
    cli_abort("This forecaster only works with daily, weekly, or yearmonth data.")
  }
  if (!inherits(args_list, c("climate_fcast", "alist"))) {
    cli_abort("`args_list` was not created using `climate_args_list()`.")
  }
  arg_is_chr_scalar(outcome)
  hardhat::check_column_names(epi_data, c(outcome, args_list$quantile_by_key))
  forecast_date <- args_list$forecast_date %||% attr(epi_data, "metadata")$as_of
  horizon <- args_list$forecast_horizon
  window_size <- args_list$window_size
  time_type <- args_list$time_type
  ttype_ord <- match(time_type, c("day", "epiweek", "week", "month"))
  ttype_ord <- ttype_ord - as.integer(ttype_ord > 2)
  edf_ttype_ord <- match(edf_time_type, c("day", "week", "yearmonth"))
  if (ttype_ord < edf_ttype_ord) {
    cli_abort(c("Climate forecasts for more granular time types are not possible
      if the `epi_data` has a higher level of aggregation",
      i = "Here, the data is in {.val {edf_time_type}}s while
      `time_type` is {.val {time_type}}."
    ))
  }
  # process the time types
  epi_data <- filter(epi_data, !is.na(!!outcome))
  ttype_dur <- switch(time_type,
                      epiweek = lubridate::weeks, week = lubridate::weeks,
                      month = lubridate::months, day = lubridate::days)
  time_aggr <- switch(time_type,
                      epiweek = lubridate::epiweek, week = lubridate::week,
                      month = lubridate::month, day = lubridate::yday)
  modulus <- switch(time_type, epiweek = 53L, week = 53L, month = 12L, day = 365L)
  center_fn <- switch(args_list$center_method,
                      mean = function(x, w) mean(x, na.rm = TRUE),
                      median = function(x, w) stats::median(x, na.rm = TRUE))
  # get the point predictions
  outcome <- sym(outcome)
  keys <- key_colnames(epi_data, exclude = "time_value")
  epi_data <- epi_data %>% mutate(.idx = time_aggr(time_value), .weights = 1)
  climate_center <- epi_data %>%
    select(.idx, .weights, outcome, all_of(keys)) %>%
    dplyr::reframe(
      roll_modular_multivec(outcome, .idx, .weights, center_fn, window_size,
                            modulus),
      .by = keys
    ) %>%
    rename(.pred = climate_pred)
  # get the quantiles
  Quantile <- function(x) {
    x <- x - stats::median(x, na.rm = TRUE)
    if (args_list$symmetrize) x <- c(x, -x)
    list(unname(quantile(
      x, probs = args_list$quantile_levels, na.rm = TRUE, type = 8
    )))
  }
  climate_quantiles <- epi_data %>%
    select(.idx, .weights, outcome, all_of(args_list$quantile_by_key)) %>%
    dplyr::reframe(
      roll_modular_multivec(outcome, .idx, .weights, Quantile, window_size,
                            modulus),
      .by = args_list$quantile_by_key
    ) %>%
    rename(.pred_distn = climate_pred)
  # combine them together
  climate_table <- climate_center %>%
    left_join(climate_quantiles, by = c(".idx", args_list$quantile_by_key)) %>%
    mutate(.pred_distn = .pred_distn - median(.pred_distn) + .pred)
  if (args_list$nonneg) {
    climate_table <- mutate(
      climate_table,
      .pred = snap(.pred, 0, Inf),
      .pred_distn = snap(.pred_distn, 0, Inf)
    )
  }
  # create the predictions
  predictions <- epi_data %>%
    select(all_of(keys)) %>%
    dplyr::distinct() %>%
    mutate(forecast_date = forecast_date, .idx = time_aggr(forecast_date))
  predictions <- map(horizon, ~ {
      predictions %>%
        mutate(.idx = .idx + .x, target_date = forecast_date + ttype_dur(.x))
    }) %>%
    purrr::list_rbind() %>%
    left_join(climate_table, by = c(".idx", keys)) %>%
    select(-.idx)

  structure(list(
    predictions = predictions,
    epi_workflow = epi_workflow(),
    metadata = list(
      training = attr(epi_data, "metadata"),
      forecast_created = Sys.time()
    )),
    class = c("climate_fcast", "canned_epipred")
  )
}

#' Climatological forecaster argument constructor
#'
#' @inheritParams epi_recipe
#' @param prediction_horizon Vector of integers giving the number of time steps,
#'   in units of the `time_type`,
#'   from the `reference_date` for which predictions should be produced.
#' @inheritParams step_climate
#' @inheritParams flatline_args_list
#'
#' @return A list containing updated parameter choices with class `climate_alist`.
#' @export
#' @seealso [climatological_predictor()], [step_climate()]
#'
#' @examples
#' climate_args_list()
#' climate_args_list(
#'   ahead = 0:10,
#'   quantile_levels = c(.01, .025, 1:19 / 20, .975, .99)
#' )
#'
climate_args_list <- function(
    forecast_date = NULL,
    forecast_horizon = 0:4,
    time_type = c("epiweek", "week", "month", "day"),
    center_method = c("median", "mean"),
    window_size = 3L,
    quantile_levels = c(.05, .1, .25, .5, .75, .9, .95),
    symmetrize = FALSE,
    nonneg = TRUE,
    quantile_by_key = character(0L),
    ...) {

  rlang::check_dots_empty()
  time_type <- arg_match(time_type)
  if (time_type != "epiweek") {
    cli_abort("Only epiweek forecasts are currently supported.")
  }
  center_method <- rlang::arg_match(center_method)
  arg_is_scalar(window_size, symmetrize, nonneg)
  arg_is_chr(quantile_by_key, allow_empty = TRUE)
  arg_is_scalar(forecast_date, allow_null = TRUE)
  arg_is_date(forecast_date, allow_null = TRUE)
  arg_is_nonneg_int(forecast_horizon, window_size)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(quantile_levels)
  quantile_levels <- sort(unique(c(0.5, quantile_levels)))

  structure(enlist(
    forecast_date, forecast_horizon, time_type, center_method, window_size,
    quantile_levels, symmetrize, nonneg, quantile_by_key),
    class = c("climate_fcast", "alist")
  )
}


#' @importFrom lubridate epiyear epiweek ymd
units_in_year <- function(years, units = c("epiweek", "week", "month", "day")) {
  units <- arg_match(units)
  if (units == "month") return(rep(12L, length(years)))
  if (units == "day") return(c(365, 366)[lubridate::leap_year(years) + 1])
  unitf <- switch(units, epiweek = lubridate::epiweek, week = lubridate::isoweek)
  mat <- outer(years, 25:31, function(x, y) paste0(x, "12", y))
  apply(mat, 1, function(x) max(unitf(lubridate::ymd(x))))
}

date_window_mod <- function(t0, time_value, left = 1L, right = 1L,
                            type = c("epiweek", "isoweek", "month", "day")) {
  checkmate::assert_integerish(left, lower = 0, len = 1)
  checkmate::assert_integerish(right, lower = 0, len = 1)
  type <- arg_match(type)
  unitf <- switch(
    type,
    epiweek = lubridate::epiweek,
    iso = lubridate::isoweek,
    month = lubridate::month,
    day = lubridate::day
  )
  yearf <- switch(
    type,
    epiweek = lubridate::epiyear,
    isoweek = lubridate::isoyear,
    month = lubridate::year,
    day = lubridate::year
  )
  cur_years <- yearf(unique(c(t0, time_value)))
  ll <- vctrs::vec_recycle_common(t0 = t0, time_value = time_value)
  t0 <- ll$t0
  time_value <- ll$time_value

  back_years <- cur_years - 1
  all_years <- sort(unique(c(cur_years, back_years)))
  year_nunits <- units_in_year(all_years, type)
  back_nunits <- year_nunits[match(yearf(time_value) - 1, all_years)]
  forward_nunits <- year_nunits[match(yearf(time_value), all_years)]

  unit_diff <- unitf(t0) - unitf(time_value)
  in_window <- ((unit_diff >= 0) & (unit_diff %% back_nunits <= left)) |
    ((unit_diff <= 0) & (unit_diff %% forward_nunits <= right))

  in_window
}


episeason <- function(time_value) {
  time_value <- time_value - lubridate::dmonths(6)
  paste0(
    strftime(time_value, "%Y"),
    "/",
    strftime(time_value + lubridate::years(1), "%y")
  )
}

season_week <- function(time_value, season_start_epiweek = 39) {
  stopifnot(season_start_epiweek >= 0L, season_start_epiweek <= 53L)
  time_value <- time_value - lubridate::weeks(season_start_epiweek)
  lubridate::epiweek(time_value)
}
