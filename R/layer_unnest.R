#' Unnest prediction list-cols
#'
#' For any model that produces forecasts for multiple outcomes, such as multiple
#' aheads, the resulting prediction is a list of forecasts inside a column of
#' the prediction tibble, which is not an ideal format. This layer "lengthens"
#' the result, moving each outcome to a separate row, in the same manner as
#' `tidyr::unnest()` would. At the moment, the only such engine is
#' `smooth_quantile_reg()`.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' aheads <- 1:7
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = aheads) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(
#'   r,
#'   smooth_quantile_reg(
#'     quantile_levels = c(.05, .1, .25, .5, .75, .9, .95),
#'     outcome_locations = aheads
#'   )
#' ) %>%
#'   fit(jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit() %>%
#'   layer_unnest(.pred)
#'
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- forecast(wf1)
#' p
layer_unnest <- function(frosting, ..., id = rand_id("unnest")) {
  arg_is_chr_scalar(id)

  add_layer(
    frosting,
    layer_unnest_new(
      terms = enquos(...),
      id = id
    )
  )
}

layer_unnest_new <- function(terms, id) {
  layer("unnest", terms = terms, id = id)
}

#' @export
slather.layer_unnest <-
  function(object, components, workflow, new_data, ...) {
    rlang::check_dots_empty()
    exprs <- rlang::expr(c(!!!object$terms))
    pos <- tidyselect::eval_select(exprs, components$predictions)
    col_names <- names(pos)
    components$predictions <- components$predictions %>%
      tidyr::unnest(col_names)

    components
  }

#' @export
print.layer_unnest <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Unnesting prediction list-cols"
  print_layer(x$terms, title = title, width = width)
}
