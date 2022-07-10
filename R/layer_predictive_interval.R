#' Returns predictive interval distributions
#'
#' This function calculates an _approximation_ to a parametric predictive
#' interval. Predictive intervals from linear models require `x* (X'X)^{-1} x*`
#' along with the degrees of freedom. This function approximates both. It
#' should be reasonably accurate for models fit using `lm` when the new point
#' `x*` isn't too far from the bulk of the data.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param dist_type Gaussian or Student's t predictive intervals
#' @param truncate Do we truncate the distribution to an interval
#' @param .flag a logical to determine if the layer is added. Passed on to
#'   `add_layer()`. Default `TRUE`.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with additional columns of the
#'   residual quantiles added to the prediction

#' @export
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>%
#'  parsnip::fit(jhu)
#'
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_predictive_interval() %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
layer_predictive_interval <- function(frosting,
                                      ...,
                                      dist_type = c("gaussian", "student_t"),
                                      truncate = c(-Inf, Inf),
                                      .flag = TRUE, # mandatory
                                      id = rand_id("predictive_interval")) {
  rlang::check_dots_empty()
  dist_type <- match.arg(dist_type)
  stopifnot(length(truncate) == 2L,
            is.numeric(truncate),
            truncate[1] < truncate[2])
  add_layer(
    frosting,
    layer_predictive_interval_new(
      dist_type = dist_type,
      truncate = truncate,
      id = id
    ),
    flag = .flag
  )
}

layer_predictive_interval_new <- function(dist_type, truncate, id) {
  layer("predictive_interval", dist_type = dist_type, truncate = truncate, id = id)
}

#' @export
slather.layer_predictive_interval <-
  function(object, components, the_fit, the_recipe, ...) {

    m <- components$predictions$.pred
    r <- grab_residuals(the_fit, components)
    df <- the_fit$df.residual
    n <- sum(!is.na(r))
    papprox <- ncol(components$mold$predictors) + 1
    if (is.null(df)) df <- n - papprox
    mse <- sum(r^2, na.rm = TRUE) / df
    s <- sqrt(mse * (1 + papprox / df )) # E[x (X'X)^1 x] if E[X'X] ~= (n-p) I
    dstn <- switch(
      object$dist_type,
      gaussian = distributional::dist_normal(m, s),
      student_t = distributional::dist_student_t(df, list(m), s)
    )
    truncate <- object$truncate
    if (!all(is.infinite(truncate))) {
      dstn <- distributional::dist_truncated(dstn, truncate[1], truncate[2])
    }
  components$predictions$.pred_int <- dstn
  components
}
