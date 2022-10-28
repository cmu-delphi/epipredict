#' Returns predictive quantiles
#'
#' This function calculates quantiles when the prediction was _distributional_.
#' Currently, the only distributional engine is `quantile_reg()`.
#' If this engine is used, then this layer will grab out estimated (or extrapolated)
#' quantiles at the requested levels.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param levels a vector of probabilities (quantiles) to extract
#' @param truncate Do we truncate the distribution to an interval
#' @param name character. The name for the output column.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor. An additional column of predictive
#'   quantiles will be added to the predictions.
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
#' wf <- epi_workflow(r, quantile_reg(tau = c(.25, .5, .75))) %>%
#'  parsnip::fit(jhu)
#'
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_quantile_distn() %>%
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
  layer("quantile_distn",
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
          "These are of class {class(dstn)}."))
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
