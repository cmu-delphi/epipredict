#' Postprocessing step to add the forecast date
#'
#' @param frosting a `frosting` postprocessor
#' @param forecast_date The forecast date to add as a column to the `epi_df`.
#' For most cases, this should be specified in the form "yyyy-mm-dd". Note that
#' when the forecast date is left unspecified, it is set to the maximum time
#' value from the data used in pre-processing, fitting the model, and
#' postprocessing.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#'
#' @details To use this function, either specify a forecast date or leave the
#'  forecast date unspecifed here. In the latter case, the forecast date will
#'  be set as the maximum time value from the data used in pre-processing,
#'  fitting the model, and postprocessing. In any case, when the forecast date is
#'  less than the maximum `as_of` value (from the data used pre-processing,
#'  model fitting, and postprocessing), an appropriate warning will be thrown.
#'
#' @export
#' @examples
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#' wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
#' latest <- jhu %>%
#'   filter(time_value >= max(time_value) - 14)
#'
#' # Don't specify `forecast_date` (by default, this should be last date in latest)
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit(.pred)
#' wf0 <- wf %>% add_frosting(f)
#' p0 <- predict(wf0, latest)
#' p0
#'
#' # Specify a `forecast_date` that is greater than or equal to `as_of` date
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_forecast_date(forecast_date = "2022-05-31") %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p1 <- predict(wf1, latest)
#' p1
#'
#' # Specify a `forecast_date` that is less than `as_of` date
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_forecast_date(forecast_date = "2021-12-31") %>%
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- predict(wf2, latest)
#' p2
#'
#' # Do not specify a forecast_date
#' f3 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_add_forecast_date() %>%
#'   layer_naomit(.pred)
#' wf3 <- wf %>% add_frosting(f3)
#'
#' p3 <- predict(wf3, latest)
#' p3
layer_add_forecast_date <-
  function(frosting, forecast_date = NULL, id = rand_id("add_forecast_date")) {
    arg_is_chr_scalar(id)
    arg_is_scalar(forecast_date, allow_null = TRUE)
    # can't validate the type of forecast_date until we know the time_type
    add_layer(
      frosting,
      layer_add_forecast_date_new(
        forecast_date = forecast_date,
        id = id
      )
    )
  }

layer_add_forecast_date_new <- function(forecast_date, id) {
  layer("add_forecast_date", forecast_date = forecast_date, id = id)
}

#' @export
slather.layer_add_forecast_date <- function(object, components, workflow,
                                            new_data, ...) {
  rlang::check_dots_empty()
  if (is.null(object$forecast_date)) {
    max_time_value <- as.Date(max(
      workflows::extract_preprocessor(workflow)$max_time_value,
      workflow$fit$meta$max_time_value,
      max(new_data$time_value)
    ))
    forecast_date <- max_time_value
  } else {
    forecast_date <- object$forecast_date
  }

  expected_time_type <- attr(
    workflows::extract_preprocessor(workflow)$template, "metadata"
  )$time_type
  if (expected_time_type == "week") expected_time_type <- "day"
  if (expected_time_type == "integer") expected_time_type <- "year"
  validate_date(
    forecast_date, expected_time_type,
    call = rlang::expr(layer_add_forecast_date())
  )
  forecast_date <- coerce_time_type(forecast_date, expected_time_type)
  object$forecast_date <- forecast_date
  components$predictions <- bind_cols(
    components$predictions,
    forecast_date = forecast_date
  )

  components
}

#' @export
print.layer_add_forecast_date <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Adding forecast date"
  fd <- ifelse(is.null(x$forecast_date), "<calculated>",
    as.character(x$forecast_date)
  )
  fd <- rlang::enquos(fd)
  print_layer(fd, title = title, width = width)
}
