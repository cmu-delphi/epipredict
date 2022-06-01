#' A wrapper function for making predictions such that if `new_data`
#' has class `epi_df`, then the keys are kept in the prediction
#'
#' @param obj A model object for which prediction is desired.
#' @param new_data Input data for which to predict values.
#' @param ahead A numeric value indicating how far ahead in the future
#'  to make forecasts.
#' @param forecast_date The date to which these forecasts correspond.
#'  Default is `NULL`.
#' @param time_value the time value associated with each row of measurements.
#'
#' @return An object of class `epi_df` with all keys from `new_data`.
#'
#' @details To use this function properly either specify an ahead value (and
#'  leave the forecast date unspecifed) or specify a forecast date. In the
#'  former, the forecast date will be set as the maximum time value plus
#'  the ahead value. In that case, as well as in the case when the
#'  forecast date is less than the most recent update date of the data
#'  (ie. the `as_of` value), an appropriate warning will be thrown.
#'
#' @export
#'
#' @examples
#' set.seed(2034)
#' n <- 100
#' tib <- tibble(
#'   x = rnorm(n), y = x + rnorm(n),
#'   time_value = seq(as.Date("2020-01-01"), by = 1, length.out = n),
#'   geo_value = "ca"
#' ) %>% epiprocess::as_epi_df(as_of = "2020-04-12")
#'
#' obj <- lm(y ~ x, data = tib)
#' newdata <- tib %>%
#'  slice_tail(n = 1)
#'
#' # Now let's predict under various circumstances
#' # ahead specified and forecast_date = NULL
#' epidf_predict(obj, newdata, ahead = 7)
#' # forecast_date = as_of
#' epidf_predict(obj, newdata, forecast_date = "2020-04-12")
#' # max_time_value < as_of < forecast_date
#' epidf_predict(obj, newdata, forecast_date = "2020-04-14")
#' # forecast_date < max_time_value < as_of
#' epidf_predict(obj, newdata, forecast_date = "2020-04-08")
#' # max time_value < forecast_date < as_of
#' epidf_predict(obj, newdata, forecast_date = "2020-04-11")

epidf_predict <- function(obj, new_data, ahead, forecast_date = NULL) {
  if (is_epi_df(new_data)) {
    pred_df <- stats::predict(obj, new_data)
    keys_df <- new_data %>%
      dplyr::select(epi_keys(new_data)) %>%
      group_by(geo_value) %>%
      dplyr::slice(n())

    max_time_value <- max(keys_df$time_value)
    as_of_date <-
      as.Date(str_extract(
        attributes(new_data)$metadata$as_of,
        "\\d{4}-\\d{2}-\\d{2}"
      ))

    if (is.null(forecast_date)) {
      forecast_date <- max_time_value + ahead
      warning("Set forecast_date equal to maximum time value plus ahead value.")
    }
    if (forecast_date < as_of_date) {
      warning("forecast_date is less than the most recent update date of the data.")
    }

    keys_df_fcd <- keys_df %>%
      dplyr::mutate(time_value = as.Date(forecast_date))

    pred_df <- as_epi_df(dplyr::bind_cols(keys_df_fcd, pred = pred_df),
                         geo_type = attributes(new_data)$metadata$geo_type,
                         time_type = attributes(new_data)$metadata$time_type,
                         as_of = attributes(new_data)$metadata$as_of)
  } else {
    pred_df <- stats::predict(obj, new_data)
  }
  pred_df
}
