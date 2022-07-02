#' Postprocessing step to add the target date
#'
#' @param frosting a `frosting` postprocessor
#' @param ahead A positive integer to add to `time_value` to get the target date.
#' This must be specified by the user.
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
#'   layer_add_target_date() %>% layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
layer_add_target_date <-
  function(frosting, id = rand_id("add_target_date")) {
    add_layer(
      frosting,
      layer_add_target_date_new(
        id = id
      )
    )
  }

layer_add_target_date_new <- function(id = id) {
  layer("add_target_date", id = id)
}

#' @export
slather.layer_add_target_date <- function(object, components, ...) {
  ahead <- as.numeric(stringr::str_extract(names(components$mold$outcomes),
                                           "(?<=ahead_)\\d+"))

  if(is.na(ahead)) stop("`ahead` must be specified in preprocessing.")
  components$predictions <- dplyr::bind_cols(components$predictions,
                                             target_date = ahead + components$predictions$time_value)
  components
}
