layer_{{ name }} <- function(frosting, # mandatory
                             ...,
                             args, # add as many as you need
                             more_args,
                             id = rand_id("{{{ name }}}")) {

  # validate any additional arguments here

  # if you don't need ... then uncomment the line below
  ## rlang::check_dots_empty()
  add_layer(
    frosting,
    layer_{{{ name }}}_new(
      terms = dplyr::enquos(...), # remove if ... should be empty
      args,
      id = id
    )
  )
}

layer_{{{ name }}}_new <- function(terms, args, more_args, id) {
  layer("{{{ name }}}",
        terms = terms,
        args = args,
        more_args = more_args,
        id = id)
}

#' @export
slather.layer_{{{ name }}} <-
  function(object, components, the_fit, the_recipe, ...) {

    # if layer_ used ... in tidyselect, we need to evaluate it now
    exprs <- rlang::expr(c(!!!object$terms))
    pos <- tidyselect::eval_select(exprs, components$predictions)
    col_names <- names(pos)
    # now can select with `tidyselect::all_of(col_names)`

    # add additional necessary processing steps here

    # always return components
    components
  }
