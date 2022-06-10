layer_predict <-
  function(frosting, id = rand_id("predict_default")) {
    add_layer(
      frosting,
      layer_predict_new(
        id = id
      )
    )
  }


layer_predict_new <- function(id) {
  layer("predict", id = id)
}

#' @export
slather.layer_predict <- function(object, components, the_fit) {
  components$predictions <- predict(the_fit, components$forged$predictors)
  components$predictions <- dplyr::bind_cols(components$keys, components$predictions)
  components
}

