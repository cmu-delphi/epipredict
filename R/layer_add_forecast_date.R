#' Postprocessing step to add the forecast date
#'
#' @param frosting a `frosting` postprocessor
#' @param forecast_date The forecast date to add as a column to the `epi_df`.
#' This must be specified by the user in the form "yyyy-mm-dd".
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   recipes::step_naomit(recipes::all_predictors()) %>%
#'   recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% parsnip::fit(jhu)
#' latest <- jhu %>%
#'   dplyr::filter(time_value >= max(time_value) - 14)
#'
#' f <- frosting() %>% layer_predict() %>%
#'   layer_add_forecast_date(forecast_date = "2021-12-31") %>% layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
layer_add_forecast_date <-
  function(frosting, forecast_date = NULL) {
    add_layer(
      frosting,
      layer_add_forecast_date_new(
        forecast_date = forecast_date
      )
    )
  }

layer_add_forecast_date_new <- function(forecast_date, id = rand_id("add_forecast_date")) {
  layer("add_forecast_date", forecast_date = forecast_date, id = id)
}

#' @export
slather.layer_add_forecast_date <- function(object, components, ...) {
  if(is.na(lubridate::ymd(object$forecast_date))) stop("specified `forecast_date` must be of format yyyy-mm-dd")

  last_rows_df <- new_data %>% # %% new_data for forecast_date needed here... perhaps this is components or components$forged$predictors here???
    dplyr::group_by(geo_value) %>%
    dplyr::slice(dplyr::n())

  max_time_value <- max(last_rows_df$time_value)

  as_of_date <-
    as.Date(stringr::str_extract(
      attributes(new_data)$metadata$as_of, # %% new_data change
      "\\d{4}-\\d{2}-\\d{2}"
    ))

  if (is.null(object$forecast_date)) {
      forecast_date <- max_time_value + ahead ## %% need to be able to access recipe to get ahead here, yes?
    warning("Set forecast_date equal to maximum time value plus ahead value.")
  }
  if (object$forecast_date < as_of_date) {
    warning("forecast_date is less than the most recent update date of the data.")
  }

  components$predictions <- dplyr::bind_cols(components$predictions,
                                             forecast_date = as.Date(object$forecast_date))
  components
}
