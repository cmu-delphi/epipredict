#' Lower and upper thresholds for predicted values
#'
#' This postprocessing step is used to set prediction values that are
#' smaller than the lower threshold or higher than the upper threshold equal
#' to the threshold values.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables. Typical usage is `.pred` to
#'   threshold predictions to a range (say, nonnegative).
#' @param lower Lower threshold for the prediction values. That is, any
#'   predictions that are less than this lower bound are set to it.
#'   Default value is `0`.
#' @param upper Upper threshold for the prediction values. That is, any
#'   predictions that are greater than this upper bound are set to it.
#'   Default value is `Inf`.
#' @param .flag a logical to determine if the layer is added. Passed on to
#'   `add_layer()`. Default `TRUE`.
#' @param id a random id string
#'
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' library(dplyr)
#' library(recipes)
#'
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value < "2021-03-08", geo_value %in% c("ak", "ca", "ar"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_naomit(all_predictors()) %>%
#'   step_naomit(all_outcomes(), skip = TRUE)
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>%
#'   parsnip::fit(jhu)
#'
#' latest <- jhu %>%
#'   filter(time_value >= max(time_value) - 14)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_threshold(.pred, lower = 0.180, upper = 0.310)
#' wf <- wf %>% add_frosting(f)
#' p <- predict(wf, latest)
#' p
layer_threshold <-
  function(frosting, ..., lower = 0, upper = Inf, .flag = TRUE,
           id = rand_id("threshold")) {
    add_layer(
      frosting,
      layer_threshold_new(
        terms = dplyr::enquos(...),
        lower = lower,
        upper = upper,
        id = id
      ),
      flag = .flag
    )
  }


layer_threshold_new <-
  function(terms, lower, upper, id = rand_id("threshold")) {
    layer("threshold", terms = terms, lower = lower, upper = upper, id = id)
  }

snap <- function(x, lower, upper) {
  arg_is_scalar(lower, upper)
  pmin(pmax(x, lower), upper)
}

#' @export
slather.layer_threshold <- function(object, components, the_fit, ...) {
  exprs <- rlang::expr(c(!!!object$terms))
  pos <- tidyselect::eval_select(exprs, components$predictions)
  col_names <- names(pos)
  components$predictions <- components$predictions %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(col_names),
        ~ snap(.x, object$lower, object$upper)
      ))
  components
}
