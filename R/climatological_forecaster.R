#' Climatological forecaster
#'
#' This is another "baseline" type forecaster, but it is especially appropriate
#' for strongly seasonal diseases (e.g., influenza). The idea is to predict
#' the "typical season" by summarizing over all available history in the
#' `epi_data`. This is analogous to a "climate" forecast rather than a "weather"
#' forecast, essentially predicting "typical January" behavior by relying on a
#' long history of such periods rather than heavily using recent data.
#'
#' The point forecast is either the mean or median of the `outcome` in a small
#' window around the target period, computed over the entire available history,
#' separately for each key in the `epi_df` (`geo_value` and any additional keys).
#' The forecast quantiles are computed from the residuals for this point prediction.
#' By default, the residuals are ungrouped, meaning every key will have the same
#' shape distribution (though different centers). Note that if your data is not
#' or comparable scales across keys, this default is likely inappropriate. In that
#' case, you can choose by which keys quantiles are computed using
#' `climate_args_list(quantile_by_key = ...)`.
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
#' cases <- cases_deaths_subset
#' # set as_of to the last day in the data
#' # "case_rate_7d_av" is on the same scale for all geographies
#' attr(cases, "metadata")$as_of <- as.Date("2021-12-31")
#' fcast <- climatological_forecaster(cases, "case_rate_7d_av")
#' autoplot(fcast)
#'
#' # Compute quantiles separately by location, and a backcast
#' # "cases" is on different scales by geography, due to population size
#' # so, it is better to compute quantiles separately
#' backcast <- climatological_forecaster(
#'   cases, "case_rate_7d_av",
#'   climate_args_list(
#'     quantile_by_key = "geo_value",
#'     forecast_date = as.Date("2021-06-01")
#'   )
#' )
#' autoplot(backcast)
#'
#' # compute the climate "daily" rather than "weekly"
#' # use a two week window (on both sides)
#' # "cases" is on different scales by geography, due to population size
#' daily_fcast <- climatological_forecaster(
#'   cases, "cases",
#'   climate_args_list(
#'     quantile_by_key = "geo_value",
#'     time_type = "day",
#'     window_size = 14L,
#'     forecast_horizon = 0:30
#'   )
#' )
#' autoplot(daily_fcast) +
#'   ggplot2::coord_cartesian(xlim = c(as.Date("2021-10-01"), NA))
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
  # check that the prediction time type is more granular than epi_data's
  # time_type
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
  sym_outcome <- sym(outcome)
  epi_data <- epi_data %>%
    filter(!is.na(!!outcome)) %>%
    select(all_of(c(key_colnames(epi_data), outcome)))
  if (time_type %in% c("week", "epiweek")) {
    ttype_dur <- lubridate::weeks
    time_aggr <- ifelse(time_type == "week", epiweek_leap, isoweek_leap)
    modulus <- 52L
  } else if (time_type == "month") {
    ttype_dur <- function(x) lubridate::period(month = x)
    time_aggr <- lubridate::month
    modulus <- 12L
  } else if (time_type == "day") {
    ttype_dur <- lubridate::days
    time_aggr <- yday_leap
    modulus <- 365L
  }
  center_fn <- switch(args_list$center_method,
    mean = function(x, w) mean(x, na.rm = TRUE),
    median = function(x, w) stats::median(x, na.rm = TRUE)
  )
  keys <- key_colnames(epi_data, exclude = "time_value")
  # Get the prediction geo and .idx for the target date(s)
  predictions <- epi_data %>%
    select(all_of(keys)) %>%
    dplyr::distinct() %>%
    mutate(forecast_date = forecast_date, .idx = time_aggr(forecast_date))
  predictions <-
    map(horizon, ~ {
      predictions %>%
        mutate(.idx = .idx + .x, target_date = forecast_date + ttype_dur(.x))
    }) %>%
    purrr::list_rbind() %>%
    mutate(
      .idx = .idx %% modulus,
      .idx = dplyr::case_when(.idx == 0 ~ modulus, TRUE ~ .idx)
    )
  # get the distinct .idx for the target date(s)
  distinct_target_idx <- predictions$.idx %>% unique()
  # get all of the idx's within the window of the target .idxs
  entries <- map(distinct_target_idx, function(idx) within_window(idx, window_size, modulus)) %>%
    do.call(c, .) %>%
    unique()
  # for the center, we need those within twice the window, since for each point
  # we're subtracting out the center to generate the quantiles
  entries_double_window <- map(entries, function(idx) within_window(idx, window_size, modulus)) %>%
    do.call(c, .) %>%
    unique()

  epi_data_target <-
    epi_data %>%
    mutate(.idx = time_aggr(time_value), .weights = 1)
  # get the point predictions
  climate_center <-
    epi_data_target %>%
    filter(.idx %in% entries_double_window) %>%
    mutate(.idx = time_aggr(time_value), .weights = 1) %>%
    select(.idx, .weights, all_of(c(outcome, keys))) %>%
    dplyr::reframe(
      roll_modular_multivec(
        !!sym_outcome, .idx, .weights, center_fn, window_size,
        modulus
      ),
      .by = all_of(keys)
    ) %>%
    rename(.pred = climate_pred)
  # get the quantiles
  Quantile <- function(x, w) {
    if (args_list$symmetrize) x <- c(x, -x)
    list(unname(quantile(
      x,
      probs = args_list$quantile_levels, na.rm = TRUE, type = 8
    )))
  }
  # add on the centers and subtract them out before computing the quantiles
  climate_quantiles <-
    epi_data_target %>%
    filter(.idx %in% entries) %>%
    left_join(climate_center, by = c(".idx", keys)) %>%
    mutate({{ outcome }} := !!sym_outcome - .pred) %>%
    select(.idx, .weights, all_of(c(outcome, args_list$quantile_by_key))) %>%
    dplyr::reframe(
      roll_modular_multivec(
        !!sym_outcome, .idx, .weights, Quantile, window_size,
        modulus
      ),
      .by = all_of(args_list$quantile_by_key)
    ) %>%
    mutate(.pred_distn = hardhat::quantile_pred(do.call(rbind, climate_pred), args_list$quantile_levels)) %>%
    select(-climate_pred)
  # combine them together
  climate_table <- climate_center %>%
    inner_join(climate_quantiles, by = c(".idx", args_list$quantile_by_key)) %>%
    mutate(.pred_distn = .pred_distn + .pred)
  predictions <- predictions %>%
    left_join(climate_table, by = c(".idx", keys)) %>%
    select(-.idx)
  if (args_list$nonneg) {
    predictions <- predictions %>% mutate(
      .pred = snap(.pred, 0, Inf),
      .pred_distn = snap(.pred_distn, 0, Inf)
    )
  }

  # fill in some extras for plotting methods, etc.
  ewf <- epi_workflow()
  ewf$trained <- TRUE
  ewf$original_data <- epi_data
  ewf$pre <- list(mold = list(
    outcomes = select(epi_data, !!sym_outcome),
    extras = list(roles = list(
      geo_value = select(epi_data, geo_value),
      time_value = select(epi_data, time_value)
    ))
  ))
  other_keys <- key_colnames(epi_data, exclude = c("time_value", "geo_value"))
  if (length(other_keys) > 0) {
    ewf$pre$mold$extras$roles$key <- epi_data %>% select(all_of(other_keys))
  }

  structure(
    list(
      predictions = predictions,
      epi_workflow = ewf,
      metadata = list(
        training = attr(epi_data, "metadata"),
        forecast_created = Sys.time()
      )
    ),
    class = c("climate_fcast", "canned_epipred")
  )
}

#' Climatological forecaster argument constructor
#'
#' @inheritParams epi_recipe
#' @param forecast_horizon Vector of integers giving the number of time steps,
#'   in units of the `time_type`,
#'   from the `reference_date` for which predictions should be produced.
#' @inheritParams step_climate
#' @inheritParams flatline_args_list
#'
#' @return A list containing updated parameter choices with class `climate_alist`.
#' @export
#' @seealso [climatological_forecaster()], [step_climate()]
#'
#' @examples
#' climate_args_list()
#' climate_args_list(
#'   forecast_horizon = 0:10,
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
  center_method <- rlang::arg_match(center_method)
  arg_is_scalar(window_size, symmetrize, nonneg)
  arg_is_chr(quantile_by_key, allow_empty = TRUE)
  arg_is_scalar(forecast_date, allow_null = TRUE)
  arg_is_date(forecast_date, allow_null = TRUE)
  arg_is_nonneg_int(window_size)
  arg_is_int(forecast_horizon)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(quantile_levels)
  quantile_levels <- sort(unique(c(0.5, quantile_levels)))

  structure(
    enlist(
      forecast_date, forecast_horizon, time_type, center_method, window_size,
      quantile_levels, symmetrize, nonneg, quantile_by_key
    ),
    class = c("climate_fcast", "alist")
  )
}

#' @export
print.climate_fcast <- function(x, ...) {
  name <- "ARX Forecaster"
  NextMethod(name = name, ...)
}
