#' Lower and upper thresholds for prediction values
#'
#' This postprocessing step is used to retain only those
#' prediction values that are within the specified lower and upper thresholds.
#'
#' @param frosting a `frosting` postprocessor
#' @param pred_lower Lower threshold for the prediction values. That is, any
#' predictions greater than or equal to this lower bound are retained.
#' Default value is `0`.
#' @param pred_upper Upper threshold for the prediction values. That is, any
#' predictions less than or equal to this upper bound are retained.
#' Default value is `Inf`.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
layer_nonneg_preds <-
  function(frosting, pred_lower = 0, pred_upper = Inf, ..., id = rand_id("nonneg_preds")) {
    add_layer(
      frosting,
      layer_nonneg_preds_new(
        pred_lower = pred_lower,
        pred_upper = pred_upper,
        id = id
      )
    )
  }


layer_nonneg_preds_new <- function(pred_lower, pred_upper, id) {
  layer("nonneg_preds", pred_lower = pred_lower, pred_upper = pred_upper, id = id)
}

#' @export
slather.layer_nonneg_preds <- function(object, components, the_fit, ...) {
  components$predictions <- components$predictions %>%
    dplyr::filter(.pred >= object$pred_lower & .pred <= object$pred_upper)
  components
}
