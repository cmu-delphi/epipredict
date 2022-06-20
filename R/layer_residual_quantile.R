#' Creates prediction intervals based on residual quantiles
#'
#' @param frosting a `frosting` postprocessor
#' @param probs numeric vector of probabilities with values in [0,1] referring to the desired quantile.
#' @param symmetrize logical. If `TRUE` then interval will be symmetrical.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with additional columns of the residual quantiles added to the prediction
#' @export
#' @examples TO-DO
layer_residual_quantile <- function(frosting, probs, symmetrize, id = rand_id("residual_quantile")) {
  add_layer(
    frosting,
    layer_residual_quantile_new(
      probs = probs,
      symmetrize = symmetrize,
      id = id
    )
  )
}

layer_residual_quantile_new <- function(terms, id) {
  layer("residual_quantile", probs = probs, symmetrize = symmetrize, id = id)
}

#' @export
slather.layer_residual_quantile <- function(object, components, the_fit,...) {
  if (is.null(object$probs)) return(components)

  s <- ifelse(object$symmetrize, -1, NA)
  r <- the_fit$fit$fit$fit$residuals
  q <- quantile(c(r, s * r), probs = probs, na.rm = TRUE)

  estimate <- components$predictions$.pred
  interval <- data.frame(outer(estimate, q, "+"))
  names(interval)<- probs_to_string(object$probs)
  components$predictions <- dplyr::bind_cols(components$predictions,interval)
  components

}
