#' Unnest prediction list-cols
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
layer_unnest <- function(frosting, ..., id = rand_id("unnest")) {
  arg_is_chr_scalar(id)

  add_layer(
    frosting,
    layer_unnest_new(
      terms = dplyr::enquos(...),
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
