#' Post-processing step to add the target date
#'
#' @param frosting a `frosting` postprocessor
#' @param target_date The target date to add as a column to the `epi_df`. If
#'   there's a forecast date specified upstream (either in a
#'   `step_adjust_latency` or in a `layer_forecast_date`), then it is the
#'   forecast date plus `ahead` (from `step_epi_ahead` in the `epi_recipe`).
#'   Otherwise, it is the maximum `time_value` (from the data used in
#'   pre-processing, fitting the model, and post-processing) plus `ahead`, where
#'   `ahead` has been specified in preprocessing. The user may override these by
#'   specifying a target date of their own (of the form "yyyy-mm-dd").
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#'
#' @details By default, this function assumes that a value for `ahead` has been
#'   specified in a preprocessing step (most likely in `step_epi_ahead`). Then,
#'   `ahead` is added to the `forecast_date` in the test data to get the target
#'   date. `forecast_date` itself can be set in 3 ways:
#' 1. The default `forecast_date` is simply the maximum `time_value` over every
#'   dataset used (prep, training, and prediction).
#' 2. if `step_adjust_latency` is present, it will typically use the training
#'   `epi_df`'s `as_of`
#' 3. `layer_add_forecast_date`, which inherits from 2 if not manually specifed
#'
#' @export
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
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
#' # Use ahead + forecast_date from adjust_latency
#' # setting the `as_of` to something realistic
#' attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_adjust_latency(method = "extend_ahead") %>%
#'   step_epi_naomit()
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_target_date() %>%
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- forecast(wf2)
#' p2
#'
#' # Use ahead + max time value from pre, fit, post
#' # which is the same if include `layer_add_forecast_date()`
#' f3 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_target_date() %>%
#'   layer_naomit(.pred)
#' wf3 <- wf %>% add_frosting(f3)
#'
#' p3 <- forecast(wf2)
#' p2
#'
#' # Specify own target date
#' f4 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_target_date(target_date = "2022-01-08") %>%
#'   layer_naomit(.pred)
#' wf4 <- wf %>% add_frosting(f4)
#'
#' p4 <- forecast(wf4)
#' p4
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
  if (expected_time_type == "integer") expected_time_type <- "year"

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
    forecast_date <- get_forecast_date_in_layer(
      extract_preprocessor(workflow),
      workflow$fit$meta$max_time_value,
      new_data
    )
    ahead <- extract_argument(the_recipe, "step_epi_ahead", "ahead")
    target_date <- forecast_date + ahead
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
