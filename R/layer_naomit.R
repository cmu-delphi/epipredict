#' Omit `NA`s from predictions or other columns
#'
#' @param frosting a `frosting` postprocessor
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables. Typical usage is `.pred` to remove
#'   any rows with `NA` predictions.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#'  jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   recipes::step_naomit(recipes::all_predictors()) %>%
#'   recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>%
#'  parsnip::fit(jhu)
#'
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f <- epipredict:::frosting() %>%
#'      layer_predict() %>%
#'      layer_naomit(.pred)
#'
#' wf1 <- wf %>% epipredict:::add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
layer_naomit <- function(frosting, ..., id = rand_id("naomit")) {
  add_layer(
    frosting,
    layer_naomit_new(
      terms = dplyr::enquos(...),
      id = id
    )
  )
}

layer_naomit_new <- function(terms, id) {
  layer("naomit", terms = terms, id = id)
}

#' @export
slather.layer_naomit <- function(object, components, the_fit,...) {
  exprs <- rlang::expr(c(!!!object$terms))
  pos <- tidyselect::eval_select(exprs, components$predictions)
  col_names <- names(pos)
  components$predictions <- components$predictions %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(col_names), ~ !is.na(.x)))
  components
}
