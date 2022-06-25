#' Creates predictions based on residual quantiles
#'
#' @param frosting a `frosting` postprocessor
#' @param probs numeric vector of probabilities with values in (0,1) referring to the desired quantile.
#' @param symmetrize logical. If `TRUE` then interval will be symmetrical.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with additional columns of the residual quantiles added to the prediction
#' @export
#' @examples
#'  jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   recipes::step_naomit(recipes::all_predictors()) %>%
#'   recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>%
#'  parsnip::fit(jhu)
#'
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f <- epipredict:::frosting() %>%
#'      layer_predict() %>%
#'      layer_residual_quantile(probs = c(0.0275, 0.975), symmetrize = FALSE) %>%
#'      layer_naomit(.pred)
#' wf1 <- wf %>% epipredict:::add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
layer_residual_quantile <- function(frosting,
                                    probs = c(0.0275, 0.975),
                                    symmetrize = TRUE,
                                    id = rand_id("residual_quantile")) {
  add_layer(
    frosting,
    layer_residual_quantile_new(
      probs = probs,
      symmetrize = symmetrize,
      id = id
    )
  )
}

layer_residual_quantile_new <- function(probs, symmetrize, id) {
  layer("residual_quantile", probs = probs, symmetrize = symmetrize, id = id)
}

#' @export
slather.layer_residual_quantile <- function(object, components, the_fit,...) {
  if (is.null(object$probs)) return(components)

  s <- ifelse(object$symmetrize, -1, NA)
  r <- the_fit$fit$residuals
  q <- quantile(c(r, s * r), probs = object$probs, na.rm = TRUE)

  estimate <- components$predictions$.pred
  interval <- data.frame(outer(estimate, q, "+"))
  names(interval)<- probs_to_string(object$probs)
  components$predictions <- dplyr::bind_cols(components$predictions,interval)
  components


}
