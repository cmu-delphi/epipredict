#' Creates predictions based on residual quantiles
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param probs numeric vector of probabilities with values in (0,1)
#'   referring to the desired quantile.
#' @param symmetrize logical. If `TRUE` then interval will be symmetric.
#' @param name character. The name for the output column.
#' @param .flag a logical to determine if the layer is added. Passed on to
#'   `add_layer()`. Default `TRUE`.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with additional columns of the
#'   residual quantiles added to the prediction
#' @export
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
#'   layer_residual_quantiles(probs = c(0.0275, 0.975), symmetrize = FALSE) %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
layer_residual_quantiles <- function(frosting, ...,
                                     probs = c(0.0275, 0.975),
                                     symmetrize = TRUE,
                                     name = ".pred_distn",
                                     .flag = TRUE,
                                     id = rand_id("residual_quantiles")) {
  rlang::check_dots_empty()
  arg_is_chr_scalar(name, id)
  arg_is_probabilities(probs)
  arg_is_lgl(symmetrize)
  add_layer(
    frosting,
    layer_residual_quantiles_new(
      probs = probs,
      symmetrize = symmetrize,
      name = name,
      id = id
    ),
    flag = .flag
  )
}

layer_residual_quantiles_new <- function(probs, symmetrize, name, id) {
  layer("residual_quantiles", probs = probs, symmetrize = symmetrize,
        name = name, id = id)
}

#' @export
slather.layer_residual_quantiles <-
  function(object, components, the_fit, the_recipe, ...) {
    if (is.null(object$probs)) return(components)

    s <- ifelse(object$symmetrize, -1, NA)
    r <- grab_residuals(the_fit, components)
    q <- quantile(c(r, s * r), probs = object$probs, na.rm = TRUE)

    estimate <- components$predictions$.pred
    res <- tibble::tibble(
      .pred_distn = dist_quantiles(map(estimate, "+", q), object$probs))
    res <- check_pname(res, components$predictions, object)
    components$predictions <- dplyr::mutate(components$predictions, !!!res)
    components
  }

grab_residuals <- function(the_fit, components) {
  if (the_fit$spec$mode != "regression")
    rlang::abort("For meaningful residuals, the predictor should be a regression model.")

  yhat <- predict(the_fit, new_data = components$mold$predictors)
  c(components$mold$outcomes - yhat)[[1]]
}
