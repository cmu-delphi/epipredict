#' Postprocessing step to add the target date
#'
#' @param frosting a `frosting` postprocessor
#' @param target_date The target date to add as a column to the
#' `epi_df`. If there's a forecast date specified in a layer, then
#' it is the forecast date plus `ahead` (from `step_epi_ahead` in
#' the `epi_recipe`). Otherwise, it is the maximum `time_value`
#' (from the data used in pre-processing, fitting the model, and
#' postprocessing) plus `ahead`, where `ahead` has been specified in
#'  preprocessing. The user may override these by specifying a
#' target date of their own (of the form "yyyy-mm-dd").
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#'
#' @details By default, this function assumes that a value for `ahead`
#' has been specified in a preprocessing step (most likely in
#' `step_epi_ahead`). Then, `ahead` is added to the maximum `time_value`
#' in the test data to get the target date.
#'
#' @export
#' @examples
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
#'
#' # Use ahead + forecast date
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_forecast_date(forecast_date = as.Date("2022-05-31")) %>%
#'   layer_add_target_date() %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- forecast(wf1)
#' p
#'
#' # Use ahead + max time value from pre, fit, post
#' # which is the same if include `layer_add_forecast_date()`
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_target_date() %>%
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- forecast(wf2)
#' p2
#'
#' # Specify own target date
#' f3 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_target_date(target_date = "2022-01-08") %>%
#'   layer_naomit(.pred)
#' wf3 <- wf %>% add_frosting(f3)
#'
#' p3 <- forecast(wf3)
#' p3
layer_add_target_date <-
  function(frosting, target_date = NULL, id = rand_id("add_target_date")) {
    arg_is_chr_scalar(id)
    arg_is_scalar(target_date, allow_null = TRUE)
    # can't validate the type of target_date until we know the time_type
    add_layer(
      frosting,
      layer_add_target_date_new(
        target_date = target_date,
        id = id
      )
    )
  }

layer_add_target_date_new <- function(id = id, target_date = target_date) {
  layer("add_target_date", target_date = target_date, id = id)
}

#' @export
slather.layer_add_target_date <- function(object, components, workflow,
                                          new_data, ...) {
  rlang::check_dots_empty()
  the_recipe <- workflows::extract_recipe(workflow)
  the_frosting <- extract_frosting(workflow)

  expected_time_type <- attr(
    workflows::extract_preprocessor(workflow)$template, "metadata"
  )$time_type
  if (expected_time_type == "week") expected_time_type <- "day"

  if (!is.null(object$target_date)) {
    target_date <- object$target_date
    validate_date(
      target_date, expected_time_type,
      call = expr(layer_add_target_date())
    )
    target_date <- coerce_time_type(target_date, expected_time_type)
  } else if (
    detect_layer(the_frosting, "layer_add_forecast_date") &&
      !is.null(forecast_date <- extract_argument(
        the_frosting, "layer_add_forecast_date", "forecast_date"
      ))) {
    validate_date(
      forecast_date, expected_time_type,
      call = rlang::expr(layer_add_forecast_date())
    )
    forecast_date <- coerce_time_type(forecast_date, expected_time_type)
    ahead <- extract_argument(the_recipe, "step_epi_ahead", "ahead")
    target_date <- forecast_date + ahead
  } else {
    max_time_value <- as.Date(max(
      workflows::extract_preprocessor(workflow)$max_time_value,
      workflow$fit$meta$max_time_value,
      max(new_data$time_value)
    ))
    ahead <- extract_argument(the_recipe, "step_epi_ahead", "ahead")
    target_date <- max_time_value + ahead
  }

  object$target_date <- target_date
  components$predictions <- bind_cols(
    components$predictions,
    target_date = target_date
  )
  components
}

#' @export
print.layer_add_target_date <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Adding target date"
  td <- ifelse(is.null(x$target_date), "<calculated>",
    as.character(x$target_date)
  )
  td <- rlang::enquos(td)
  print_layer(td, title = title, width = width)
}
