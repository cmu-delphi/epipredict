#' Prediction layer for postprocessing
#'
#' Implements prediction on a fitted `epi_workflow`. One may want different
#' types of prediction, and to potentially apply this after some amount of
#' postprocessing. This would typically be the first layer in a `frosting`
#' postprocessor.
#'
#' @seealso `parsnip::predict.model_fit()`
#'
#' @inheritParams parsnip::predict.model_fit
#' @param frosting a frosting object
#' @param id a string identifying the layer
#'
#'
#' @return An updated `frosting` object
#' @export
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' latest <- jhu %>% filter(time_value >= max(time_value) - 14)
#'
#' # Predict layer alone
#' f <- frosting() %>% layer_predict()
#' wf1 <- wf %>% add_frosting(f)
#'
#' p1 <- predict(wf1, latest)
#' p1
#'
#' # Prediction with interval
#' f <- frosting() %>% layer_predict(type = "pred_int")
#' wf2 <- wf %>% add_frosting(f)
#'
#' p2 <- predict(wf2, latest)
#' p2
layer_predict <-
  function(frosting, type = NULL, opts = list(), ...,
           id = rand_id("predict_default")) {
    arg_is_chr_scalar(id)
    arg_is_chr_scalar(type, allow_null = TRUE)
    add_layer(
      frosting,
      layer_predict_new(
        type = type,
        opts = opts,
        dots_list = rlang::list2(...), # can't figure how to use this
        id = id
      )
    )
  }


layer_predict_new <- function(type, opts, dots_list, id) {
  layer("predict", type = type, opts = opts, dots_list = dots_list, id = id)
}

#' @export
slather.layer_predict <- function(object, components, workflow, new_data, ...) {
  the_fit <- workflows::extract_fit_parsnip(workflow)

  components$predictions <- predict(
    the_fit,
    components$forged$predictors,
    type = object$type, opts = object$opts
  )
  components$predictions <- dplyr::bind_cols(
    components$keys, components$predictions
  )
  components
}

#' @export
print.layer_predict <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Creating predictions"
  td <- "<calculated>"
  td <- rlang::enquos(td)
  print_layer(td, title = title, width = width)
}
