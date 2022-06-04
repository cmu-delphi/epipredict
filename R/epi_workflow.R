#' @export
epi_workflow <- function(preprocessor = NULL, spec = NULL) {
  out <- workflow(preprocessor, spec)
  class(out) <- c("epi_workflow", class(out))
}

predict.epi_workflow <-
  function(object, new_data, type = NULL, opts = list(), forecast_date, ...) {
    out <- predict(object, new_data, type = type, opts = opts, ...)
    if (is_epi_df(new_data)) {
      ek <- epi_keys(new_data)

    }
  }

is_epi_workflow <- function(x) {
  inherits(x, "epi_workflow")
}

workflow <- function(preprocessor = NULL, spec = NULL) {
  out <- new_workflow()

  if (!is_null(preprocessor)) {
    out <- add_preprocessor(out, preprocessor)
  }

  if (!is_null(spec)) {
    out <- add_model(out, spec)
  }

  out
}

add_preprocessor <- function(x, preprocessor, ..., call = caller_env()) {
  check_dots_empty()

  if (is_formula(preprocessor)) {
    return(add_formula(x, preprocessor))
  }

  if (is_recipe(preprocessor)) {
    return(add_recipe(x, preprocessor))
  }

  if (is_workflow_variables(preprocessor)) {
    return(add_variables(x, variables = preprocessor))
  }

  abort(
    "`preprocessor` must be a formula, recipe, or a set of workflow variables.",
    call = call
  )
}
