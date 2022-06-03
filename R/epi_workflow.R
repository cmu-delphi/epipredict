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
