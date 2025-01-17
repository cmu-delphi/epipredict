#' Climatological forecaster
#'
#' This is another "baseline" type forecaster, but it is especially appropriate
#' for strongly seasonal diseases (e.g., influenza). The idea is to forecast
#' the "typical season" by summarizing over all available history in the
#' `epi_data`. This is analogous to a "climate" forecast rather than a "weather"
#' forecast, essentially predicting "typical January" behavior by relying on a
#' long history of such periods rather than heavily using recent data.
#'
#' The forecast is taken as the quantiles of the `outcome` in a small window
#' around the target week, computed over the entire available history.
#'
#' @inheritParams flatline_forecaster
#' @param args_list A list of additional arguments as created by the
#'   [climate_args_list()] constructor function.
#'
#' @return A data frame of point and interval) forecasts at a all horizons
#'  for each unique combination of `key_vars`.
#' @export
#'
#' @examples
#'
#' 1 + 1
#' 2 + 2
climatological_forecaster <- function(
    epi_data,
    outcome,
    args_list = climate_args_list()
) {
  if (!is_epi_df(epi_data)) {
    cli_abort(c(
      "`epi_data` must be an {.cls epi_df}.",
      "!" = "This one is a {.cls {class(epi_data)}}."
    ))
  }
  if (!inherits(args_list, c("climate_fcast", "alist"))) {
    cli_abort("`args_list` was not created using `climate_args_list()`.")
  }
  arg_is_chr_scalar(outcome)
  hardhat::check_column_names(epi_data, c(outcome, args_list$quantile_by_key))
  ahead <- args_list$ahead
  window_size <- args_list$window_size
  names(ahead) <- paste0(".ahead_", ahead)
  ahead_tib <- tibble(.ahead_set = names(ahead), ahead = ahead)
  forecast_date <- lubridate::ymd(args_list$forecast_date) %||%
    max(epi_data$time_value) %||%
    attributes(epi_data)$metadata$as_of
  probs <- switch(args_list$quantile_method,
                  epipredict = c(.25, .5, .75),
                  base8 = args_list$quantile_values)
  outcome <- sym(outcome)
  epi_data <- filter(epi_data, !is.na(!!outcome))
  target_dates <- forecast_date + lubridate::weeks(ahead)
  in_window <- map2(
    target_dates, names(ahead),
    ~ tibble(!!.y := date_window_mod(.x, epi_data$time_value, window_size, window_size))
  )

  epi_data <- bind_cols(epi_data, purrr::list_cbind(in_window)) %>%
    pivot_longer(
      dplyr::starts_with(".ahead_"), names_to = ".ahead_set",
      values_to = ".in_window"
    ) %>%
    filter(.in_window) %>%
    select(-.in_window) %>%
    left_join(ahead_tib, by = ".ahead_set")

  Quantile <- function(x) {
    x <- x - median(x, na.rm = TRUE)
    if (args_list$symmetrize) x <- c(x, -x)
    list(unname(quantile(x, probs = probs, na.rm = TRUE, type = 8)))
  }
  quantiles <- group_by(epi_data, .ahead_set, ahead) %>%
    summarise(
      .pred_distn = dist_quantiles(Quantile(!!outcome), probs),
      .by = ,
      .groups = "drop"
    )

  epi_data <- epi_data %>%
    group_by(.ahead_set, ahead, args_list$quantile_by_key) %>%
    summarise(.pred = median(!!outcome, na.rm = TRUE), .groups = "drop") %>%
    left_join(quantiles, by = c(".ahead_set", "ahead")) %>%
    mutate(
      .pred_distn = .pred_distn + .pred,
      forecast_date = ymd(forecast_date),
      target_date = epiweek(forecast_date + weeks(ahead)),
      .ahead_set = NULL
    )

  if (args_list$quantile_method == "epipredict") {
    epi_data <- mutate(
      epi_data,
      .pred_distn = extrapolate_quantiles(.pred_distn, args_list$quantile_values)
    )
  }
  if (args_list$nonneg) {
    epi_data <- mutate(
      epi_data,
      .pred = snap(.pred, 0, Inf),
      .pred_distn = snap(.pred_distn, 0, Inf)
    )
  }
  structure(list(
    predictions = epi_data,
    metadata = list(
      training = attr(epi_data, "metadata"),
      forecast_created = Sys.time()
    )),
    class = c("climate_fcast", "canned_epipred")
  )
}

#' Climatological forecaster argument constructor
#'
#' @inheritParams flatline_args_list
#' @param ahead Vector of integers giving the number of time steps ahead
#'   (in weeks) of the forecast date for which forecasts should be produced.
#' @param time_type Character. The duration to which the forecasts correspond.
#' @param window_size Integer. The number of time points on each side of the
#'   target to include in the calculation.
#' @param quantile_method One of either `"base8"` or `"epipredict"`. The first
#'   case uses the quantiles of the observed history within the window, calculated
#'   using `type = 8`. See `?stats::quantile` for additional information.
#'   Alternatively, `"epipredict"` computes only the quartiles of the observed
#'   data and interpolates (or extrapolates the remainder). Those quantiles
#'   between .25 and .75 are interpolated with a cubic spline, while those
#'   outside this range are extrapolated on the logistic-linear scale. This
#'   produces a "parametric" quantile estimate with tails that are heavier than
#'   a normal distribution. See the Examples for this comparison.
#'
#' @return A list containing updated parameter choices with class `climate_alist`.
#' @export
#'
#' @examples
#'
#' climate_args_list()
#' climate_args_list(
#'   ahead = 0:10,
#'   quantile_levels = c(.01, .025, 1:19 / 20, .975, .99)
#' )
#'
#' # To visualize the quantiles produced with the `epipredict` method
#' tau <- c(.01, .025, 1:19 / 20, .975, .99)
#' sm_tau <- 5:15 / 20 # values between .25 and .75
#' distn <- dist_quantiles(qnorm(sm_tau), sm_tau)
#' epipredict_quantiles <- extrapolate_quantiles(distn, tau) %>%
#'   nested_quantiles %>%
#'   purrr::pluck(1, "values")
#'
#' plot(
#'   qnorm(tau),
#'   epipredict_quantiles,
#'   pch = 16, col = 4, ylab = "epipredict", xlab = "Normal quantiles"
#' )
#' abline(0, 1, col = 2)
climate_args_list <- function(
    ahead = 1:4,
    forecast_date = NULL,
    time_type = c("epiweek", "week", "month", "year", "day"),
    window_size = 3L,
    quantile_method = c("base8", "epipredict"),
    quantile_levels = c(.1, .25, .5, .75, .9),
    symmetrize = FALSE,
    nonneg = TRUE,
    quantile_by_key = character(0L),
    ...) {

  rlang::check_dots_empty()
  time_type <- arg_match(time_type)
  quantile_method <- arg_match(quantile_method)
  if (time_type != "epiweek") {
    cli_abort("Only epiweek forecasts are currently supported.")
  }
  arg_is_scalar(ahead, window_size, symmetrize, nonneg)
  arg_is_chr(quantile_by_key, allow_empty = TRUE)
  arg_is_scalar(forecast_date, allow_null = TRUE)
  arg_is_date(forecast_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, window_size)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(quantile_levels)

  structure(enlist(
    ahead, forecast_date, time_type, window_size, quantile_method,
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
