#' Get test data for prediction based on longest lag period
#'
#' Based on the longest lag period in the recipe,
#' `get_predict_data()` creates an [epi_df][epiprocess::as_epi_df]
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
#' get_predict_data(recipe = rec, x = covid_case_death_rates)
#' @importFrom rlang %@%
#' @importFrom stats na.omit
#' @export
get_predict_data <- function(recipe,
                          x,
                          test_interval = NULL,
                          reference_date = NULL) {
  if (!is_epi_df(x)) cli_abort("`x` must be an `epi_df`.")
  check <- hardhat::check_column_names(x, colnames(recipe$template))
  if (!check$ok) {
    cli_abort(c(
      "Some variables used for training are not available in {.arg x}.",
      i = "The following required columns are missing: {check$missing_names}"
    ))
  }
  reference_date <- reference_date %||% recipe$reference_date
  test_interval <- test_interval %||% as.difftime(365, units = "days")
  trimmed_x <- x %>%
    filter((reference_date - time_value) < test_interval)

  if (nrow(trimmed_x) == 0) {
    cli_abort(
      "predict data is filtered to no rows; check your `test_interval = {test_interval}` and `reference_date= {reference_date}`",
      class = "epipredict__get_predict_data__no_predict_data"
    )
  }

  trimmed_x
}
