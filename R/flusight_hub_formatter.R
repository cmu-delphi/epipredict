location_to_abbr <- function(location) {
  dictionary <-
    epidatasets::state_census %>%
    dplyr::transmute(
      location = dplyr::case_match(fips, "00" ~ "US", .default = fips),
      abbr
    )
  dictionary$abbr[match(location, dictionary$location)]
}

abbr_to_location <- function(abbr) {
  dictionary <-
    epidatasets::state_census %>%
    dplyr::transmute(
      location = dplyr::case_match(fips, "00" ~ "US", .default = fips),
      abbr
    )
  dictionary$location[match(abbr, dictionary$abbr)]
}




#' Format predictions for submission to FluSight forecast Hub
#'
#' This function converts predictions from any of the included forecasters into
#' a format (nearly) ready for submission to the 2023-24
#' [FluSight-forecast-hub](https://github.com/cdcepi/FluSight-forecast-hub).
#' See there for documentation of the required columns. Currently, only
#' "quantile" forcasts are supported, but the intention is to support both
#' "quantile" and "pmf". For this reason, adding the `output_type` column should
#' be done via the `...` argument. See the examples below. The specific required
#' format for this forecast task is [here](https://github.com/cdcepi/FluSight-forecast-hub/blob/main/model-output/README.md).
#'
#' @param object a data.frame of predictions or an object of class
#'   `canned_epipred` as created by, e.g., [arx_forecaster()]
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name = value pairs of constant
#'   columns (or mutations) to perform to the results. See examples.
#' @param .fcast_period Control whether the `horizon` should represent days or
#'   weeks. Depending on whether the forecaster output has target dates
#'   from [layer_add_target_date()] or not, we may need to compute the horizon
#'   and/or the `target_end_date` from the other available columns in the predictions.
#'   When both `ahead` and `target_date` are available, this is ignored. If only
#'   `ahead` or `aheads` exists, then the target date may need to be multiplied
#'   if the `ahead` represents weekly forecasts. Alternatively, if only, the
#'   `target_date` is available, then the `horizon` will be in days, unless
#'   this argument is `"weekly"`. Note that these can be adjusted later by the
#'   `...` argument.
#'
#' @return A [tibble::tibble]. If `...` is empty, the result will contain the
#'   columns `reference_date`, `horizon`, `target_end_date`, `location`,
#'   `output_type_id`, and `value`. The `...` can perform mutations on any of
#'   these.
#' @export
#'
#' @examples
#' library(dplyr)
#' library(epiprocess)
#' weekly_deaths <- covid_case_death_rates %>%
#'   filter(
#'     time_value >= as.Date("2021-09-01"),
#'     geo_value %in% c("ca", "ny", "dc", "ga", "vt")
#'   ) %>%
#'   select(geo_value, time_value, death_rate) %>%
#'   left_join(epidatasets::state_census %>% select(pop, abbr), by = c("geo_value" = "abbr")) %>%
#'   mutate(deaths = pmax(death_rate / 1e5 * pop * 7, 0)) %>%
#'   select(-pop, -death_rate) %>%
#'   group_by(geo_value) %>%
#'   epi_slide(~ sum(.$deaths), .window_size = 7, .new_col_name = "deaths_7dsum") %>%
#'   ungroup() %>%
#'   filter(weekdays(time_value) == "Saturday")
#'
#' cdc <- cdc_baseline_forecaster(weekly_deaths, "deaths_7dsum")
#' flusight_hub_formatter(cdc)
#' flusight_hub_formatter(cdc, target = "wk inc covid deaths")
#' flusight_hub_formatter(cdc, target = paste(horizon, "wk inc covid deaths"))
#' flusight_hub_formatter(cdc, target = "wk inc covid deaths", output_type = "quantile")
flusight_hub_formatter <- function(
    object, ...,
    .fcast_period = c("daily", "weekly")) {
  UseMethod("flusight_hub_formatter")
}

#' @export
flusight_hub_formatter.canned_epipred <- function(
    object, ...,
    .fcast_period = c("daily", "weekly")) {
  flusight_hub_formatter(object$predictions, ..., .fcast_period = .fcast_period)
}

#' @export
flusight_hub_formatter.data.frame <- function(
    object, ...,
    .fcast_period = c("daily", "weekly")) {
  required_names <- c(".pred", ".pred_distn", "forecast_date", "geo_value")
  optional_names <- c("ahead", "target_date")
  hardhat::validate_column_names(object, required_names)
  if (!any(optional_names %in% names(object))) {
    cli_abort("At least one of {.val {optional_names}} must be present.")
  }

  dots <- enquos(..., .named = TRUE)
  names <- names(dots)

  object <- object %>%
    # combine the predictions and the distribution
    pivot_quantiles_longer(.pred_distn) %>%
    # now we create the correct column names
    rename(
      value = .pred_distn_value,
      output_type_id = .pred_distn_quantile_level,
      reference_date = forecast_date
    ) %>%
    # convert to fips codes, and add any constant cols passed in ...
    mutate(location = abbr_to_location(tolower(geo_value)), geo_value = NULL)

  # create target_end_date / horizon, depending on what is available
  pp <- ifelse(match.arg(.fcast_period) == "daily", 1L, 7L)
  has_ahead <- charmatch("ahead", names(object))
  if ("target_date" %in% names(object) && !is.na(has_ahead)) {
    object <- object %>%
      rename(
        target_end_date = target_date,
        horizon = !!names(object)[has_ahead]
      )
  } else if (!is.na(has_ahead)) { # ahead present, not target date
    object <- object %>%
      rename(horizon = !!names(object)[has_ahead]) %>%
      mutate(target_end_date = horizon * pp + reference_date)
  } else { # target_date present, not ahead
    object <- object %>%
      rename(target_end_date = target_date) %>%
      mutate(horizon = as.integer((target_end_date - reference_date)) / pp)
  }
  object %>%
    relocate(
      reference_date, horizon, target_end_date, location, output_type_id, value
    ) %>%
    mutate(!!!dots)
}
