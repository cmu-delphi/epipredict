#' Lower and upper thresholds for prediction values
#'
#' This postprocessing step is used to set prediction values that are
#' samaller than the lower threshold or higher than the upper threshold equal
#' to the threshold values.
#'
#' @param frosting a `frosting` postprocessor
#' @param pred_lower Lower threshold for the prediction values. That is, any
#' predictions that are less than this lower bound are set to it.
#' Default value is `0`.
#' @param pred_upper Upper threshold for the prediction values. That is, any
#' predictions that are greater than this upper bound are set to it.
#' Default value is `Inf`.
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value < "2021-03-08", geo_value %in% c("ak", "ca", "ar"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   recipes::step_naomit(recipes::all_predictors()) %>%
#'   recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% parsnip::fit(jhu)
#' latest <- jhu %>%
#'   dplyr::filter(time_value >= max(time_value) - 14)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_nonneg_preds(pred_lower = 0.180, pred_upper = 0.310)
#' wf1 <- wf %>% add_frosting(f)
#' p <- predict(wf1, latest)
#' p
layer_nonneg_preds <-
  function(frosting, pred_lower = 0, pred_upper = Inf) {
    add_layer(
      frosting,
      layer_nonneg_preds_new(
        pred_lower = pred_lower,
        pred_upper = pred_upper
      )
    )
  }


layer_nonneg_preds_new <- function(pred_lower, pred_upper, id = rand_id("nonneg_preds")) {
  layer("nonneg_preds", pred_lower = pred_lower, pred_upper = pred_upper, id = id)
}

#' @export
slather.layer_nonneg_preds <- function(object, components, the_fit, ...) {
  components$predictions <- components$predictions %>%
    dplyr::mutate(.pred = dplyr::case_when(.pred < object$pred_lower ~ object$pred_lower,
                                    .pred > object$pred_upper ~ object$pred_upper,
                                    TRUE ~ .pred))
  components
}
