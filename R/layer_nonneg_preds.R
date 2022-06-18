#' Lower and upper thresholds for prediction values
#'
#' This postprocessing step is used to retain only those
#' prediction values that are within the specified lower and upper thresholds.
#'
#' @param frosting a `frosting` postprocessor
#' @param pred_lower Lower threshold for the prediction values. That is, any
#' predictions greater than or equal to this lower bound are retained.
#' Default value is `0`.
#' @param pred_upper Upper threshold for the prediction values. That is, any
#' predictions less than or equal to this upper bound are retained.
#' Default value is `Inf`.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   recipes::step_naomit(recipes::all_predictors()) %>%
#'   recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% parsnip::fit(jhu)
#' latest <- jhu %>%
#'   dplyr::filter(time_value >= max(time_value) - 14)
#'
#' f <- epipredict:::frosting() %>%
#'   layer_predict() %>%
#'   layer_nonneg_preds(pred_lower = 0.25, pred_upper = 0.31)
#' wf1 <- wf %>% epipredict:::add_frosting(f)
#' p <- predict(wf1, latest)
#' p
layer_nonneg_preds <-
  function(frosting, pred_lower = 0, pred_upper = Inf, id = rand_id("nonneg_preds")) {
    add_layer(
      frosting,
      layer_nonneg_preds_new(
        pred_lower = pred_lower,
        pred_upper = pred_upper,
        id = id
      )
    )
  }


layer_nonneg_preds_new <- function(pred_lower, pred_upper, id) {
  layer("nonneg_preds", pred_lower = pred_lower, pred_upper = pred_upper, id = id)
}

#' @export
slather.layer_nonneg_preds <- function(object, components, the_fit, ...) {
  components$predictions <- components$predictions %>%
    dplyr::filter(.pred >= object$pred_lower & .pred <= object$pred_upper)
  components
}
