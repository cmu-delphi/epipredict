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
slather.layer_naomit <- function(object, components, the_fit) {
  exprs <- rlang::expr(c(!!!object$terms))
  pos <- tidyselect::eval_select(exprs, components$predictions)
  col_names <- names(pos)
  components$predictions <- components$predictions %>%
    dplyr::filter(dplyr::if_any(dplyr::all_of(col_names), ~ !is.na(.x)))
  components
}
