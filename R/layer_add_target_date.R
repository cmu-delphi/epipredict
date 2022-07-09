#' Postprocessing step to add the target date
#'
#' @param frosting a `frosting` postprocessor
#' @param target_date The target date to add as a column to the `epi_df`.
#' By default, this is the maximum `time_value` from the processed test
#' data plus `ahead`, where `ahead` has been specified in preprocessing
#' (most likely in `step_epi_ahead`). The user may override this with a
#' date of their own (that will usually be in the form "yyyy-mm-dd").
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#'
#' @details By default, this function assumes that a value for `ahead`
#' has been specified in a preprocessing step (most likely in
#' `step_epi_ahead`). Then, `ahead` is added to the maximum `time_value`
#' in the test data to get the target date.
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
#' # Use ahead from preprocessing
#' f <- frosting() %>% layer_predict() %>%
#'   layer_add_target_date() %>% layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
#'
#' # Override default behaviour by specifying own target date
#' f2 <- frosting() %>% layer_predict() %>%
#' layer_add_target_date(target_date = "2022-01-08") %>% layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- predict(wf2, latest)
#' p2
layer_add_target_date <-
  function(frosting, target_date = NULL, id = rand_id("add_target_date")) {
    add_layer(
      frosting,
      layer_add_target_date_new(
        target_date = target_date,
        id = id
      )
    )
  }

layer_add_target_date_new <- function(id = id, target_date = target_date) {
  layer("add_target_date",  target_date = target_date, id = id)
}

#' @export
slather.layer_add_target_date <- function(object, components, the_fit, the_recipe, ...) {

  max_time_value <- max(components$keys$time_value)

  if (is.null(object$target_date)) {
  ahead <- unlist(the_recipe$steps)$ahead

  if (is.na(ahead)) stop("`ahead` must be specified in preprocessing.")
  components$predictions <- dplyr::bind_cols(components$predictions,
                                             target_date = max_time_value + ahead)
  } else{
    components$predictions <- dplyr::bind_cols(components$predictions,
                                               target_date = as.Date(object$target_date))
  }
  components
}
