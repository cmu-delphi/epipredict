#' Get test data for prediction based on longest lag period
#'
#' Based on the longest lag period in the recipe,
#' `get_test_data()` creates an [epi_df][epiprocess::as_epi_df]
#' with columns `geo_value`, `time_value`
#' and other variables in the original dataset,
#' which will be used to create features necessary to produce forecasts.
#'
#' The minimum required (recent) data to produce a forecast is equal to
#' the maximum lag requested (on any predictor) plus the longest horizon
#' used if growth rate calculations are requested by the recipe. This is
#' calculated internally.
#'
#' @param recipe A recipe object.
#' @param x An epi_df. The typical usage is to
#'   pass the same data as that used for fitting the recipe.
#' @param test_interval A time interval or integer. The length of time before
#'   the `forecast_date` to consider for the forecast. The default is 1 year,
#'   which you will likely only need to make longer if you are doing long
#'   forecast horizons, or shorter if you are forecasting using an expensive
#'   model.
#'
#' @return An object of the same type as `x` with columns `geo_value`, `time_value`, any additional
#'   keys, as well other variables in the original dataset.
#' @examples
#' # create recipe
#' rec <- epi_recipe(covid_case_death_rates) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14))
#' get_test_data(recipe = rec, x = covid_case_death_rates)
#' @importFrom rlang %@%
#' @importFrom stats na.omit
#' @export
get_test_data <- function(recipe,
                          x,
                          test_interval,
                          reference_date = NULL) {
  if (!is_epi_df(x)) cli_abort("`x` must be an `epi_df`.")
  browser()
  names(recipe)
  reference_date <- reference_date %||% recipe$reference_date
  full_data %>%
    filter((max_time_value - time_value) < test_interval) %>%
    arrange(time_value)


  check <- hardhat::check_column_names(x, colnames(recipe$template))
  if (!check$ok) {
    cli_abort(c(
      "Some variables used for training are not available in {.arg x}.",
      i = "The following required columns are missing: {check$missing_names}"
    ))
  }

  min_lags <- min(map_dbl(recipe$steps, ~ min(.x$lag %||% Inf)), Inf)
  max_lags <- max(map_dbl(recipe$steps, ~ max(.x$lag %||% 0)), 0)
  max_horizon <- max(map_dbl(recipe$steps, ~ max(.x$horizon %||% 0)), 0)
  max_slide <- max(map_dbl(recipe$steps, ~ max(.x$before %||% 0)), 0)
  min_required <- max_lags + max_horizon + max_slide
  keep <- max_lags + max_horizon

  # CHECK: Error out if insufficient training data
  # Probably needs a fix based on the time_type of the epi_df
  avail_recent <- diff(range(x$time_value))
  if (avail_recent < keep) {
    cli_abort(c(
      "You supplied insufficient recent data for this recipe. ",
      "!" = "You need at least {min_required} days of data,",
      "!" = "but `x` contains only {avail_recent}."
    ))
  }
  max_time_value <- x %>%
    na.omit() %>%
    pull(time_value) %>%
    max()
  x <- arrange(x, time_value)
  groups <- epi_keys_only(recipe)

  # If we skip NA completion, we remove undesirably early time values
  # Happens globally, over all groups
  x <- filter(x, max_time_value - time_value <= keep)

  # If all(lags > 0), then we get rid of recent data
  if (min_lags > 0 && min_lags < Inf) {
    x <- filter(x, max_time_value - time_value >= min_lags)
  }

  filter(x, max_time_value - time_value <= keep) %>%
    epiprocess::ungroup()
}

#' get_test_data is broken, this is a hack that manually sets the amount of data
#' kept to a large interval to avoid errors/missing data
#' @param full_data the full data to narrow down from
#' @param test_data_interval the amount of time to go backwards from the last
#'   day
get_oversized_test_data <- function(full_data, test_data_interval, preproc, source_value = "nhsn") {
  # getting the max time value of data columns actually used
  non_na_indicators <- preproc$var_info %>%
    filter(role == "pre-predictor") %>%
    pull(variable)
  max_time_value <- full_data %>%
    na.omit(non_na_indicators) %>%
    pull(time_value) %>%
    max()
  if ("source" %in% names(full_data)) {
    full_data <- full_data %>% filter(source == source_value)
  }
  full_data %>%
    filter((max_time_value - time_value) < test_data_interval) %>%
    arrange(time_value)
}
