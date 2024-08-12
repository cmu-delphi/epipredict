#' Returns predictive distributions
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
#' @examples
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_predictive_distn() %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- forecast(wf1)
#' p
layer_predictive_distn <- function(frosting,
                                   ...,
                                   dist_type = c("gaussian", "student_t"),
                                   truncate = c(-Inf, Inf),
                                   name = ".pred_distn",
                                   id = rand_id("predictive_distn")) {
  rlang::check_dots_empty()
  arg_is_chr_scalar(name, id)
  dist_type <- match.arg(dist_type)
  stopifnot(
    length(truncate) == 2L, is.numeric(truncate), truncate[1] < truncate[2]
  )

  add_layer(
    frosting,
    layer_predictive_distn_new(
      dist_type = dist_type,
      truncate = truncate,
      name = name,
      id = id
    )
  )
}

layer_predictive_distn_new <- function(dist_type, truncate, name, id) {
  layer("predictive_distn",
    dist_type = dist_type, truncate = truncate,
    name = name, id = id
  )
}

#' @export
slather.layer_predictive_distn <-
  function(object, components, workflow, new_data, ...) {
    the_fit <- workflows::extract_fit_parsnip(workflow)
    rlang::check_dots_empty()

    m <- components$predictions$.pred
    r <- grab_residuals(the_fit, components)
    df <- the_fit$df.residual
    n <- sum(!is.na(r))
    papprox <- ncol(components$mold$predictors) + 1
    if (is.null(df)) df <- n - papprox
    mse <- sum(r^2, na.rm = TRUE) / df
    s <- sqrt(mse * (1 + papprox / df)) # E[x (X'X)^1 x] if E[X'X] ~= (n-p) I
    dstn <- switch(object$dist_type,
      gaussian = distributional::dist_normal(m, s),
      student_t = distributional::dist_student_t(df, m, s)
    )
    truncate <- object$truncate
    if (!all(is.infinite(truncate))) {
      dstn <- distributional::dist_truncated(dstn, truncate[1], truncate[2])
    }
    dstn <- tibble::tibble(dstn = dstn)
    dstn <- check_pname(dstn, components$predictions, object)
    components$predictions <- dplyr::mutate(components$predictions, !!!dstn)
    components
  }

#' @export
print.layer_predictive_distn <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Creating approximate predictive intervals"
  td <- "<calculated>"
  td <- rlang::enquos(td)
  print_layer(td,
    title = title, width = width, conjunction = "type",
    extra_text = x$dist_type
  )
}
