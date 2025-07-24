#' Create an epi_workflow
#'
#' This is a container object that unifies preprocessing, fitting, prediction,
#' and post-processing for predictive modeling on epidemiological data. It
#' extends the functionality of a [workflows::workflow()] to handle the typical
#' panel data structures found in this field. This extension is handled
#' completely internally, and should be invisible to the user. For all intents
#' and purposes, this operates exactly like a [workflows::workflow()]. For some
#' `{epipredict}` specific examples, see the [custom epiworkflows
#' vignette](../articles/custom_epiworkflows.html).
#'
#' @inheritParams workflows::workflow
#' @param postprocessor An optional postprocessor to add to the workflow.
#'   Currently only `frosting` is allowed using, `add_frosting()`.
#'
#' @return A new `epi_workflow` object.
#' @seealso [workflows::workflow()]
#' @importFrom rlang is_null
#' @importFrom stats predict
#' @importFrom generics fit
#' @importFrom generics augment
#' @export
#' @examples
#' jhu <- covid_case_death_rates
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg())
#'
#' wf
epi_workflow <- function(preprocessor = NULL, spec = NULL, postprocessor = NULL) {
  out <- workflows::workflow(spec = spec)
  class(out) <- c("epi_workflow", class(out))

  if (is_epi_recipe(preprocessor)) {
    out <- add_epi_recipe(out, preprocessor)
  } else if (!is_null(preprocessor)) {
    out <- workflows:::add_preprocessor(out, preprocessor)
  }
  if (!is_null(postprocessor)) {
    out <- add_postprocessor(out, postprocessor)
  }

  out
}

#' Test for an `epi_workflow`
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from `epi_workflow`.
#'
#' @keywords internal
#' @export
is_epi_workflow <- function(x) {
  inherits(x, "epi_workflow")
}


#' Fit an `epi_workflow` object
#'
#' @description
#' This is the `fit()` method for an `epi_workflow()` object that
#' estimates parameters for a given model from a set of data.
#' Fitting an `epi_workflow()` involves two main steps, which are
#' preprocessing the data and fitting the underlying parsnip model.
#'
#' @inheritParams workflows::fit.workflow
#'
#' @param object an `epi_workflow` object
#'
#' @param data an `epi_df` of predictors and outcomes to use when
#' fitting the `epi_workflow`
#'
#' @param control A [workflows::control_workflow()] object
#'
#' @return The `epi_workflow` object, updated with a fit parsnip
#' model in the `object$fit$fit` slot.
#'
#' @seealso [workflows::fit-workflow()]
#'
#' @name fit-epi_workflow
#' @export
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7)
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' wf
#'
#' @export
fit.epi_workflow <- function(object, data, ..., control = workflows::control_workflow()) {
  object$fit$meta <- list(
    max_time_value = max(data$time_value),
    as_of = attr(data, "metadata")$as_of,
    other_keys = attr(data, "metadata")$other_keys
  )
  object$original_data <- data

  res <- NextMethod()
  class(res) <- c("epi_workflow", class(res))
  res
}

#' Predict from an epi_workflow
#'
#' @description
#' This is the `predict()` method for a fit epi_workflow object. The 3 steps that this implements are:
#' - Preprocess `new_data` using the preprocessing method specified when the
#'   workflow was created and fit. This is accomplished using
#'   [hardhat::forge()], which will apply any formula preprocessing or call
#'   [recipes::bake()] if a recipe was supplied.
#'
#' - Preprocessing `new_data` using the preprocessing method specified when the
#'   epi_workflow was created and fit. This is accomplished using
#'   `hardhat::bake()` if a recipe was supplied (passing through
#'   [hardhat::forge()], which is used for non-recipe preprocessors). Note that
#'   this is a slightly different `bake` operation than the one occuring during
#'   the fit. Any `step` that has `skip = TRUE` isn't applied during prediction;
#'   for example in `step_epi_naomit()`, `all_outcomes()` isn't `NA` omitted,
#'   since doing so would drop the exact `time_values` we are trying to predict.
#'
#' - Calling `parsnip::predict.model_fit()` for you using the underlying fit
#'   parsnip model.
#'
#' - `slather()` any frosting that has been included in the `epi_workflow`.
#'
#' @param object An epi_workflow that has been fit by
#'   [workflows::fit.workflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has.
#' If `new_data` is an `epiprocess::epi_df` or a data frame with `time_value` or
#' `geo_value` columns, then the result will have those as well.
#'
#' @name predict-epi_workflow
#' @export
#' @examples
#' jhu <- covid_case_death_rates
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' latest <- jhu %>% dplyr::filter(time_value >= max(time_value) - 14)
#'
#' preds <- predict(wf, latest)
#' preds
predict.epi_workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  if (!workflows::is_trained_workflow(object)) {
    cli_abort(c(
      "Can't predict on an untrained epi_workflow.",
      i = "Do you need to call `fit()`?"
    ))
  }
  components <- list()
  components$mold <- workflows::extract_mold(object)
  components$forged <- hardhat::forge(new_data,
    blueprint = components$mold$blueprint
  )

  components$keys <- grab_forged_keys(components$forged, object, new_data)
  components <- apply_frosting(object, components, new_data, type = type, opts = opts, ...)
  components$predictions
}



#' Augment data with predictions
#'
#' `augment()`, unlike `forecast()`, has the goal of modifying the training
#' data, rather than just producing new forecasts. It does a prediction on
#' `new_data`, which will produce a prediction for most `time_values`, and then
#' adds `.pred` as a column to `new_data` and returns the resulting join.
#'
#' @param x A trained epi_workflow
#' @param new_data A epi_df of predictors
#' @param ... Arguments passed on to the predict method.
#'
#' @return new_data with additional columns containing the predicted values
#' @export
augment.epi_workflow <- function(x, new_data, ...) {
  predictions <- predict(x, new_data, ...)
  if (is_epi_df(predictions)) {
    join_by <- key_colnames(predictions)
  } else {
    cli_abort(c(
      "Cannot determine how to join `new_data` with the `predictions`.",
      "Try converting `new_data` to an {.cls epi_df} with `as_epi_df(new_data)`."
    ))
  }
  complete_overlap <- intersect(names(new_data), join_by)
  if (length(complete_overlap) < length(join_by)) {
    rlang::warn(glue::glue(
      "Your original training data had keys {join_by}, but",
      "`new_data` only has {complete_overlap}. The output",
      "may be strange."
    ))
  }
  full_join(predictions, new_data, by = join_by)
}

new_epi_workflow <- function(
    pre = workflows:::new_stage_pre(),
    fit = workflows:::new_stage_fit(),
    post = workflows:::new_stage_post(),
    trained = FALSE) {
  out <- workflows:::new_workflow(
    pre = pre, fit = fit, post = post, trained = trained
  )
  class(out) <- c("epi_workflow", class(out))
  out
}


#' @export
print.epi_workflow <- function(x, ...) {
  print_header(x)
  print_preprocessor(x)
  # workflows:::print_case_weights(x)
  print_model(x)
  print_postprocessor(x)
  invisible(x)
}


#' Produce a forecast from an epi workflow and it's training data
#'
#' `forecast.epi_workflow` predicts by restricting the training data to the
#' latest available data, and predicting on that. It binds together
#' `get_test_data()` and `predict()`.
#'
#' @param object An epi workflow.
#' @param ... Not used.
#'
#' @return A forecast tibble.
#'
#' @export
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value > "2021-08-01")
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' epi_workflow(r, parsnip::linear_reg()) %>%
#'   fit(jhu) %>%
#'   forecast()
forecast.epi_workflow <- function(object, ...) {
  if (!object$trained) {
    cli_abort(c(
      "You cannot `forecast()` a {.cls workflow} that has not been trained.",
      i = "Please use `fit()` before forecasting."
    ))
  }

  frosting_fd <- NULL
  if (has_postprocessor(object) && detect_layer(object, "layer_add_forecast_date")) {
    frosting_fd <- extract_argument(object, "layer_add_forecast_date", "forecast_date")
    if (!is.null(frosting_fd) && class(frosting_fd) != class(object$original_data$time_value)) {
      cli_abort(c(
        "Error with layer_add_forecast_date():",
        i = "The type of `forecast_date` must match the type of the `time_value` column in the data."
      ))
    }
  }

  test_data <- get_test_data(
    hardhat::extract_preprocessor(object),
    object$original_data
  )

  predict(object, new_data = test_data)
}
