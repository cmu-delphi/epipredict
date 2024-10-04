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
get_test_data <- function(recipe, x) {
  if (!is_epi_df(x)) cli_abort("`x` must be an `epi_df`.")

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
