layer_{{ name }} <- function(frosting, # mandatory
                           ...,
                           args, # add as many as you need
                           .flag = TRUE, # mandatory
                           id = rand_id("{{{ name }}}")) {

  # if you don't need ... then uncomment the below
  ## rlang::check_dots_empty()
  add_layer(
    frosting,
    layer_{{{ name }}}_new(
      args,
      id = id
    ),
    flag = .flag
  )
}

layer_{{{ name }}}_new <- function(args, id) {
  layer("{{{ name }}}", args, id = id)
}

#' @export
slather.layer_{{{ name }}} <-
  function(object, components, the_fit, the_recipe, ...) {

  # add necessary processing steps here

  # always return components
  components
}
