#' Creates quantiles of prediction residual
#'
#' @param frosting a `frosting` postprocessor
#' @param probs numeric vector of probabilities with values in [0,1]. # or should it be called level?
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples TO-DO
layer_residual_quantile <- function(frosting, probs, id = rand_id("residual_quantile")) {
  add_layer(
    frosting,
    layer_residual_quantile_new(
      terms = dplyr::enquos(...),
      id = id
    )
  )
}

layer_residual_quantile_new <- function(terms, id) {
  layer("residual_quantile", terms = terms, id = id)
}

#' @export
slather.layer_residual_quantile <- function(object, components, the_fit,...) {
 # (needs the_fit, residuals, .preds)

}
