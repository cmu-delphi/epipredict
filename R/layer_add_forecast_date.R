#' Postprocessing step to add the forecast date
#'
#' @param frosting a `frosting` postprocessor
#' @param forecast_date The forecast date to add as a column to the `epi_df`.
#' This should be specified in the form "yyyy-mm-dd".
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#'
#' @details To use this function, either specify an ahead value in
#'  preprocessing and leave the forecast date unspecifed here or simply specify
#'  a forecast date here. In the former, the forecast date will be set as the
#'  maximum time value plus the ahead value. In that case, as well as in the case
#'  when the forecast date is less than the most recent update date of the data
#'  (ie. the `as_of` value), an appropriate warning will be thrown.
#'
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
#' # Specify a `forecast_date` that is greater than or equal to `as_of` date
#' f <- frosting() %>% layer_predict() %>%
#'   layer_add_forecast_date(forecast_date = "2022-05-31") %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p1 <- predict(wf1, latest)
#' p1
#'
#' # Specify a `forecast_date` that is less than `as_of` date
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_forecast_date(forecast_date = "2021-12-31") %>%
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- predict(wf2, latest)
#' p2
#' # Do not specify a forecast_date in `layer_add_forecast_date()`
#'  f3 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_forecast_date() %>%
#'   layer_naomit(.pred)
#' wf3 <- wf %>% add_frosting(f3)
#'
#' p3 <- predict(wf3, latest)
#' p3
layer_add_forecast_date <-
  function(frosting, forecast_date = NULL, id = rand_id("add_forecast_date")) {
    add_layer(
      frosting,
      layer_add_forecast_date_new(
        forecast_date = forecast_date,
        id = id
      )
    )
  }

layer_add_forecast_date_new <- function(forecast_date, id = id) {
  layer("add_forecast_date", forecast_date = forecast_date, id = id)
}

#' @export
slather.layer_add_forecast_date <- function(object, components, the_fit, the_recipe, ...) {

  max_time_value <- max(components$keys$time_value)

  as_of_date <- as.Date.POSIXct(attributes(components$keys)$metadata$as_of)

  ahead <- the_recipe$steps[[2]][["ahead"]]

  if (is.null(object$forecast_date)) {
    object$forecast_date <- max_time_value + ahead
    warning("Set forecast_date equal to maximum time value plus ahead value.")
  }

  if (object$forecast_date < as_of_date) {
    warning("forecast_date is less than the most recent update date of the data.")
  }

  components$predictions <- dplyr::bind_cols(components$predictions,
                                             forecast_date = as.Date(object$forecast_date))
  components
}

