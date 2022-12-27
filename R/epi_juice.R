#' Extract transformed training set
#'
#' @inheritParams bake.epi_recipe
epi_juice <- function(object, ...) {
  if (!fully_trained(object)) {
    rlang::abort("At least one step has not been trained. Please run `prep()`.")
  }

  if (!isTRUE(object$retained)) {
    rlang::abort(paste0(
      "Use `retain = TRUE` in `prep()` to be able ",
      "to extract the training set"
    ))
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(dplyr::everything())
  }

  # Get user requested columns
  new_data <- object$template
  out_names <- recipes_eval_select(terms, new_data, object$term_info,
                                   check_case_weights = FALSE)
  new_data <- new_data[, out_names]

  # Since most models require factors, do the conversion from character
  if (!is.null(object$levels)) {
    var_levels <- object$levels
    var_levels <- var_levels[out_names]
    check_values <-
      vapply(var_levels, function(x) {
        (!all(is.na(x)))
      }, c(all = TRUE))
    var_levels <- var_levels[check_values]
    if (length(var_levels) > 0) {
      new_data <- recipes:::strings2factors(new_data, var_levels)
    }
  }

  new_data
}
