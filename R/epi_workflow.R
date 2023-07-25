#' Create an epi_workflow
#'
#' This is a container object that unifies preprocessing, fitting, prediction,
#' and postprocessing for predictive modeling on epidemiological data. It extends
#' the functionality of a [workflows::workflow()] to handle the typical panel
#' data structures found in this field. This extension is handled completely
#' internally, and should be invisible to the user. For all intents and purposes,
#' this operates exactly like a [workflows::workflow()]. For more details
#' and numerous examples, see there.
#'
#' @inheritParams workflows::workflow
#' @param postprocessor An optional postprocessor to add to the workflow.
#'   Currently only `frosting` is allowed using, `add_frosting()`.
#'
#' @return A new `epi_workflow` object.
#' @seealso workflows::workflow
#' @importFrom rlang is_null
#' @importFrom stats predict
#' @importFrom generics fit
#' @importFrom generics augment
#' @export
#' @examples
#' jhu <- case_death_rate_subset
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
#' This is the `fit()` method for an `epi_workflow` object that
#' estimates parameters for a given model from a set of data.
#' Fitting an `epi_workflow` involves two main steps, which are
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
#' @seealso workflows::fit-workflow
#'
#' @name fit-epi_workflow
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#' filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7)
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' wf
#'
#' @export
fit.epi_workflow <- function(object, data, ..., control = workflows::control_workflow()){

  object$fit$meta <- list(mtv = max(data$time_value), as_of = attributes(data)$metadata$as_of)

  NextMethod()
}

#' Predict from an epi_workflow
#'
#' @description
#' This is the `predict()` method for a fit epi_workflow object. The nice thing
#' about predicting from an epi_workflow is that it will:
#'
#' - Preprocess `new_data` using the preprocessing method specified when the
#'   workflow was created and fit. This is accomplished using
#'   [hardhat::forge()], which will apply any formula preprocessing or call
#'   [recipes::bake()] if a recipe was supplied.
#'
#' - Call [parsnip::predict.model_fit()] for you using the underlying fit
#'   parsnip model.
#'
#' - Ensure that the returned object is an [epiprocess::epi_df] where
#'   possible. Specifically, the output will have `time_value` and
#'   `geo_value` columns as well as the prediction.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @param object An epi_workflow that has been fit by
#'   [workflows::fit.workflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has.
#' If `new_data` is an `epi_df` or a data frame with `time_value` or
#' `geo_value` columns, then the result will have those as well.
#'
#' @name predict-epi_workflow
#' @export
#' @examples
#' jhu <- case_death_rate_subset
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
predict.epi_workflow <- function(object, new_data, ...) {
  if (!workflows::is_trained_workflow(object)) {
    rlang::abort(
      c("Can't predict on an untrained epi_workflow.",
        i = "Do you need to call `fit()`?"))
  }
  components <- list()
  components$mold <- workflows::extract_mold(object)
  components$forged <- hardhat::forge(new_data,
                                      blueprint = components$mold$blueprint)
  components$keys <- grab_forged_keys(components$forged,
                                      components$mold, new_data)
  components <- apply_frosting(object, components, new_data, ...)
  components$predictions
}



#' Augment data with predictions
#'
#' @param x A trained epi_workflow
#' @param new_data A epi_df of predictors
#' @param ... Arguments passed on to the predict method.
#'
#' @return new_data with additional columns containing the predicted values
#' @export
augment.epi_workflow <- function (x, new_data, ...) {
  predictions <- predict(x, new_data, ...)
  if (epiprocess::is_epi_df(predictions)) join_by <- epi_keys(predictions)
  else rlang::abort(
    c("Cannot determine how to join new_data with the predictions.",
      "Try converting new_data to an epi_df with `as_epi_df(new_data)`."))
  complete_overlap <- intersect(names(new_data), join_by)
  if (length(complete_overlap) < length(join_by)) {
    rlang::warn(
      glue::glue("Your original training data had keys {join_by}, but",
                 "`new_data` only has {complete_overlap}. The output",
                 "may be strange."))
  }
  dplyr::full_join(predictions, new_data, by = join_by)
}

new_epi_workflow <- function(
    pre = workflows:::new_stage_pre(),
    fit = workflows:::new_stage_fit(),
    post = workflows:::new_stage_post(),
    trained = FALSE) {

  out <- workflows:::new_workflow(
    pre = pre, fit = fit, post = post, trained = trained)
  class(out) <- c("epi_workflow", class(out))
}


#' @export
print.epi_workflow <- function(x, ...) {
  print_header(x)
  workflows:::print_preprocessor(x)
  #workflows:::print_case_weights(x)
  workflows:::print_model(x)
  print_postprocessor(x)
  invisible(x)
}

print_header <- function(x) {
  # same as in workflows but with a postprocessor
  trained <- ifelse(workflows::is_trained_workflow(x), " [trained]", "")

  header <- glue::glue("Epi Workflow{trained}")
  header <- cli::rule(header, line = 2)
  cat_line(header)

  preprocessor_msg <- cli::style_italic("Preprocessor:")

  if (workflows:::has_preprocessor_formula(x)) {
    preprocessor <- "Formula"
  } else if (workflows:::has_preprocessor_recipe(x)) {
    preprocessor <- "Recipe"
  } else if (workflows:::has_preprocessor_variables(x)) {
    preprocessor <- "Variables"
  } else {
    preprocessor <- "None"
  }

  preprocessor_msg <- glue::glue("{preprocessor_msg} {preprocessor}")
  cat_line(preprocessor_msg)

  spec_msg <- cli::style_italic("Model:")

  if (workflows:::has_spec(x)) {
    spec <- class(workflows::extract_spec_parsnip(x))[[1]]
    spec <- glue::glue("{spec}()")
  } else {
    spec <- "None"
  }

  spec_msg <- glue::glue("{spec_msg} {spec}")
  cat_line(spec_msg)

  postprocessor_msg <- cli::style_italic("Postprocessor:")
  postprocessor <- ifelse(has_postprocessor_frosting(x), "Frosting", "None")
  postprocessor_msg <- glue::glue("{postprocessor_msg} {postprocessor}")
  cat_line(postprocessor_msg)

  invisible(x)
}

