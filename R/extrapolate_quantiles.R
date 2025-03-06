#' Extrapolate the quantiles to new quantile levels
#'
#' This both interpolates between quantile levels already defined in `x` and
#' extrapolates quantiles outside their bounds. The interpolation method is
#' determined by the `quantile` argument `middle`, which can be either `"cubic"`
#' for a (hyman) cubic spline interpolation, or `"linear"` for simple linear
#' interpolation.
#'
#' There is only one extrapolation method for values greater than the largest
#' known quantile level or smaller than the smallest known quantile level. It
#' assumes a roughly exponential tail, whose decay rate and offset is derived
#' from the slope of the two most extreme quantile levels on a logistic scale.
#' See the internal function `tail_extrapolate()` for the exact implementation.
#'
#' This function takes a `quantile_pred` vector and returns the same
#' type of object, expanded to include
#' *additional* quantiles computed at `probs`. If you want behaviour more
#' similar to [stats::quantile()], then `quantile(x,...)` may be more
#' appropriate.
#'
#' @param x A vector of class `quantile_pred`.
#' @param probs a vector of probabilities at which to calculate quantiles
#' @param replace_na logical. If `x` contains `NA`'s, these are imputed if
#'   possible (if `TRUE`) or retained (if `FALSE`).
#' @param ... additional arguments passed on to the `quantile` method
#'
#' @return a `quantile_pred` vector. Each element
#'   of `x` will now have a superset
#'   of the original `quantile_values` (the union of those and `probs`).
#' @export
#'
#' @examples
#' dstn <- quantile_pred(rbind(1:4, 8:11), c(.2, .4, .6, .8))
#' # extra quantiles are appended
#' as_tibble(extrapolate_quantiles(dstn, probs = c(0.25, 0.5, 0.75)))
#'
#' extrapolate_quantiles(dstn, probs = c(0.0001, 0.25, 0.5, 0.75, 0.99999))
extrapolate_quantiles <- function(x, probs, replace_na = TRUE, ...) {
  UseMethod("extrapolate_quantiles")
}

#' @export
extrapolate_quantiles.quantile_pred <- function(x, probs, replace_na = TRUE, ...) {
  arg_is_lgl_scalar(replace_na)
  arg_is_probabilities(probs)
  if (is.unsorted(probs)) probs <- sort(probs)
  orig_probs <- x %@% "quantile_levels"
  orig_values <- as.matrix(x)

  if (!replace_na || !anyNA(orig_values)) {
    all_values <- cbind(orig_values, quantile(x, probs, ...))
  } else {
    newx <- quantile(x, orig_probs, ...) %>%
      hardhat::quantile_pred(orig_probs)
    all_values <- cbind(as.matrix(newx), quantile(newx, probs, ...))
  }
  all_probs <- c(orig_probs, probs)
  dups <- duplicated(all_probs)
  all_values <- all_values[, !dups, drop = FALSE]
  all_probs <- all_probs[!dups]
  o <- order(all_probs)

  hardhat::quantile_pred(
    all_values[, o, drop = FALSE],
    quantile_levels = all_probs[o]
  )
}
