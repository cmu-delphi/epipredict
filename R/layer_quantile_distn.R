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
#'   layer_predictive_distn() %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
layer_quantile_distn <- function(frosting,
                                 ...,
                                 levels = c(.25, .75),
                                 truncate = c(-Inf, Inf),
                                 name = ".pred_distn",
                                 id = rand_id("quantile_distn")) {
  rlang::check_dots_empty()
  arg_is_chr_scalar(name, id)
  arg_is_probabilities(levels)
  stopifnot(
    length(truncate) == 2L, is.numeric(truncate), truncate[1] < truncate[2]
  )

  add_layer(
    frosting,
    layer_quantile_distn_new(
      levels = levels,
      truncate = truncate,
      name = name,
      id = id
    ),
    flag = TRUE
  )
}

layer_quantile_distn_new <- function(levels, truncate, name, id) {
  layer("predictive_distn",
        levels = levels,
        truncate = truncate,
        name = name,
        id = id)
}

#' @export
slather.layer_quantile_distn <-
  function(object, components, the_fit, the_recipe, ...) {

    dstn <- components$predictions$.pred
    if (!inherits(dstn, "distribution")) {
      rlang::abort(
        c("`layer_quantile_distn` requires distributional predictions.",
          "These are of class {class(pred_dstn)}."))
    }
    dstn <- dist_quantiles(quantile(dstn, object$levels), object["levels"])


    truncate <- object$truncate
    if (!all(is.infinite(truncate))) {
      dstn <- snap(dstn, truncate[1], truncate[2])
    }
    dstn <- tibble::tibble(dstn = dstn)
    dstn <- check_pname(dstn, components$predictions, object)
    components$predictions <- dplyr::mutate(components$predictions, !!!dstn)
    components
  }
