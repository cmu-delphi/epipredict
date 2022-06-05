#' Create an epi_workflow
#'
#' This is a container object that unifies preprocessing, fitting, prediction,
#' and postprocessing for predictive modeling on epidemiological data. It extends
#' the functionality of a [`workflows::workflow()`] to handle the typical panel
#' data structures found in this field. This extension is handled completely
#' internally, and should be invisible to the user. For all intents and purposes,
#' this operates exactly like a [`workflows::workflow()`]. For more details
#' and numerous examples, see there.
#'
#' @inheritParams workflows::workflow
#'
#' @return A new `epi_workflow` object.
#' @seealso workflows::workflow
#' @importFrom rlang is_null
#' @export
epi_workflow <- function(preprocessor = NULL, spec = NULL) {
  out <- workflows::workflow(spec = spec)
  class(out) <- c("epi_workflow", class(out))

  if (is_epi_recipe(preprocessor)) {
    return(add_epi_recipe(out, preprocessor))
  }

  if (!is_null(preprocessor)) {
    return(workflows:::add_preprocessor(out, preprocessor))
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


predict.epi_workflow <-
  function(object, new_data, type = NULL, opts = list(),
           forecast_date = NULL, ...) {
    if (!workflows::is_trained_workflow(object)) {
      rlang::abort(
        c("Can't predict on an untrained epi_workflow.",
          i = "Do you need to call `fit()`?"))
    }
    the_fit <- workflows::extract_fit_parsnip(object)
    mold <- workflows::extract_mold(object)
    forged <- hardhat::forge(new_data, blueprint = mold$blueprint)
    preds <- predict(the_fit, forged$predictors, type = type, opts = opts, ...)
    keys <- grab_forged_keys(forged, mold, new_data)
    out <- dplyr::bind_cols(keys, preds, forecast_date)
    out
  }

grab_forged_keys <- function(forged, mold, new_data) {
  keys <- c("time_value", "geo_value", "key")
  forged_names <- names(forged$extras$roles)
  molded_names <- names(mold$extras$roles)
  extras <- dplyr::bind_cols(forged$extras$roles[forged_names %in% keys])
  # 1. these are the keys in the test data after prep/bake
  new_keys <- names(extras)
  # 2. these are the keys in the training data
  old_keys <- purrr::map_chr(mold$extras$roles[molded_names %in% keys], names)
  # 3. these are the keys in the test data as input
  new_df_keys <- epi_keys(new_data)
  if (! (setequal(old_keys, new_df_keys) && setequal(new_keys, new_df_keys))) {
    rlang::warn(c(
      "Not all epi keys that were present in the training data are available",
      "in `new_data`. Predictions will have only the available keys.")
    )
  }
  if (epiprocess::is_epi_df(new_data) || keys[1:2] %in% new_keys) {
    l <- list()
    if (length(new_keys) > 2) l <- list(other_keys = new_keys[-c(1:2)])
    extras <- as_epi_df(extras, additional_metadata = l)
  }
  extras
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
