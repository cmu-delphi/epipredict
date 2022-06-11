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
#'
#' @return A new `epi_workflow` object.
#' @seealso workflows::workflow
#' @importFrom rlang is_null
#' @importFrom stats predict
#' @importFrom generics fit
#' @importFrom generics augment
#' @export
#' @examples
#' library(dplyr)
#' library(parsnip)
#' library(recipes)
#'
#' jhu <- case_death_rate_subset
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_naomit(all_predictors()) %>%
#'   step_naomit(all_outcomes(), skip = TRUE)
#'
#' wf <- epi_workflow(r, linear_reg())
#'
#' wf
epi_workflow <- function(preprocessor = NULL, spec = NULL,
                         postprocessor = NULL) {
  out <- workflows::workflow(spec = spec)
  class(out) <- c("epi_workflow", class(out))

  if (is_epi_recipe(preprocessor)) {
    return(add_epi_recipe(out, preprocessor))
  }
  if (!is_null(preprocessor)) {
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
#' @export
is_epi_workflow <- function(x) {
  inherits(x, "epi_workflow")
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
#' @param forecast_date The date on which the forecast is (was) made.
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
#'
#' library(dplyr)
#' library(parsnip)
#' library(recipes)
#'
#' jhu <- case_death_rate_subset
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_naomit(all_predictors()) %>%
#'   step_naomit(all_outcomes(), skip = TRUE)
#'
#' wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
#'
#' latest <- get_test_data(r, jhu)
#'
#' preds <- predict(wf, latest) %>%
#'   filter(!is.na(.pred))
#'
#' preds
predict.epi_workflow <- function(object, new_data, ...) {
  if (!workflows::is_trained_workflow(object)) {
    rlang::abort(
      c("Can't predict on an untrained epi_workflow.",
        i = "Do you need to call `fit()`?"))
  }
  components <- list()
  the_fit <- workflows::extract_fit_parsnip(object)
  components$mold <- workflows::extract_mold(object)
  components$forged <- hardhat::forge(new_data,
                                      blueprint = components$mold$blueprint)
  components$keys <- grab_forged_keys(components$forged,
                                      components$mold, new_data)
  components <- apply_frosting(object, components, the_fit, ...)
  components$predictions
}

grab_forged_keys <- function(forged, mold, new_data) {
  keys <- c("time_value", "geo_value", "key")
  forged_roles <- names(forged$extras$roles)
  extras <- dplyr::bind_cols(forged$extras$roles[forged_roles %in% keys])
  # 1. these are the keys in the test data after prep/bake
  new_keys <- names(extras)
  # 2. these are the keys in the training data
  old_keys <- epi_keys_mold(mold)
  # 3. these are the keys in the test data as input
  new_df_keys <- epi_keys(new_data)
  if (! (setequal(old_keys, new_df_keys) && setequal(new_keys, new_df_keys))) {
    rlang::warn(c(
      "Not all epi keys that were present in the training data are available",
      "in `new_data`. Predictions will have only the available keys.")
    )
  }
  if (epiprocess::is_epi_df(new_data)) {
    extras <- epiprocess::as_epi_df(extras)
    attr(extras, "metadata") <- attr(new_data, "metadata")
  } else if (keys[1:2] %in% new_keys) {
    l <- list()
    if (length(new_keys) > 2) l <- list(other_keys = new_keys[-c(1:2)])
    extras <- epiprocess::as_epi_df(extras, additional_metadata = l)
  }
  extras
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
  if (is_epi_df(predictions)) join_by <- epi_keys(predictions)
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
