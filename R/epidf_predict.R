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
      warning("Set forecast_date equal to maximum time_value.")
    } else if (max_time_value <= forecast_date) {
      warning("Maximum time_value is less than or equal to forecast_date.")
    } else if (forecast_date < as_of_date) {
      warning("forecast_date is less than the most recent update date of the data (as_of).")
    }

    keys_df_fcd <- keys_df %>%
      dplyr::mutate(time_value = forecast_date) # %% called this time_value ok? Overwriting that.

    return(as_epi_df(dplyr::bind_cols(keys_df_fcd, pred_df)))
  } else {
    stats::predict(obj, new_data)
  }
}
