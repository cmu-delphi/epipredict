#' Summarize a distribution with a set of quantiles
#'
#' @param x a `distribution` vector
#' @param probs a vector of probabilities at which to calculate quantiles
#' @param ... additional arguments passed on to the `quantile` method
#'
#' @return a `distribution` vector containing `dist_quantiles`
#' @export
#'
#' @examples
#' library(distributional)
#' dstn <- dist_normal(c(10, 2), c(5, 10))
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
#'
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' # because this distribution is already quantiles, any extra quantiles are
#' # appended
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
#'
#' dstn <- c(
#'   dist_normal(c(10, 2), c(5, 10)),
#'   dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' )
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
extrapolate_quantiles <- function(x, probs, replace_na = TRUE, ...) {
  UseMethod("extrapolate_quantiles")
}

#' @export
#' @importFrom vctrs vec_data
extrapolate_quantiles.distribution <- function(x, probs, replace_na = TRUE, ...) {
  rlang::check_dots_empty()
  arg_is_lgl_scalar(replace_na)
  arg_is_probabilities(probs)
  if (is.unsorted(probs)) probs <- sort(probs)
  dstn <- lapply(vec_data(x), extrapolate_quantiles, probs = probs, replace_na = replace_na)
  new_vctr(dstn, vars = NULL, class = "distribution")
}

#' @export
extrapolate_quantiles.dist_default <- function(x, probs, replace_na = TRUE, ...) {
  values <- quantile(x, probs, ...)
  new_quantiles(values = values, quantile_levels = probs)
}

#' @export
extrapolate_quantiles.dist_quantiles <- function(x, probs, replace_na = TRUE, ...) {
  orig_probs <- field(x, "quantile_levels")
  orig_values <- field(x, "values")
  new_probs <- c(orig_probs, probs)
  dups <- duplicated(new_probs)
  if (!replace_na || !anyNA(orig_values)) {
    new_values <- c(orig_values, quantile(x, probs, ...))
  } else {
    nas <- is.na(orig_values)
    orig_values[nas] <- quantile(x, orig_probs[nas], ...)
    new_values <- c(orig_values, quantile(x, probs, ...))
  }
  new_quantiles(new_values[!dups], new_probs[!dups])
}
