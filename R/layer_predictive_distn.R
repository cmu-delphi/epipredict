#' Returns predictive distributions
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function calculates an _approximation_ to a parametric predictive
#' distribution. Predictive distributions from linear models require
#' `x* (X'X)^{-1} x*`
#' along with the degrees of freedom. This function approximates both. It
#' should be reasonably accurate for models fit using `lm` when the new point
#' `x*` isn't too far from the bulk of the data.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param dist_type Gaussian or Student's t predictive intervals
#' @param truncate Do we truncate the distribution to an interval
#' @param name character. The name for the output column.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with additional columns of the
#'   residual quantiles added to the prediction
#' @export
#'
layer_predictive_distn <- function(frosting,
                                   ...,
                                   dist_type = c("gaussian", "student_t"),
                                   truncate = c(-Inf, Inf),
                                   name = ".pred_distn",
                                   id = rand_id("predictive_distn")) {
  lifecycle::deprecate_stop("0.1.11", "layer_predictive_distn()", "layer_residual_quantiles()")
}
