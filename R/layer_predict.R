layer_predict <-
  function(frosting, type = NULL, opts = list(), ..., id = rand_id("predict_default")) {
    add_layer(
      frosting,
      layer_predict_new(
        type = type,
        opts = opts,
        dots_list = rlang::list2(...), # can't figure how to use this
        id = id
      )
    )
  }


layer_predict_new <- function(type, opts, dots_list, id) {
  layer("predict", type = type, opts = opts, dots_list = dots_list, id = id)
}

#' @export
slather.layer_predict <- function(object, components, the_fit) {

  components$predictions <- predict(the_fit, components$forged$predictors,
                                    type = object$type, opts = object$opts)
  components$predictions <- dplyr::bind_cols(components$keys, components$predictions)
  components
}

