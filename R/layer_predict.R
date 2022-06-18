#' Prediction layer for postprocessing
#'
#' Implements prediction on a fitted `epi_workflow`. One may want different
#' types of prediction, and to potentially apply this after some amount of
#' postprocessing. This would typically be the first layer in a `frosting`
#' postprocessor.
#'
#' @seealso `parsnip::predict.model_fit()`
#'
#' @inheritParams parsnip::predict.model_fit
#' @param frosting a frosting object
#' @param id a string identifying the layer
#'
#' @return An updated `frosting` object
#' @export
layer_predict <-
  function(frosting, type = NULL, opts = list(), ..., id = rand_id("nonneg_preds")) {
    add_layer(
      frosting,
      layer_nonneg_preds_new(
        pred_lower = pred_lower,
        pred_upper = pred_upper,
        id = id
      )
    )
  }


layer_predict_new <- function(type, opts, dots_list, id) {
  layer("predict", type = type, opts = opts, dots_list = dots_list, id = id)
}

#' @export
slather.layer_predict <- function(object, components, the_fit, ...) {

  components$predictions <- predict(the_fit, components$forged$predictors,
                                    type = object$type, opts = object$opts)
  components$predictions <- dplyr::bind_cols(components$keys, components$predictions)
  components
}

