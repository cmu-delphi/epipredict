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

slather.layer_naomit <- function(object, components, the_fit) {
  components$predictions <- components$predictions %>%
    filter()
}
