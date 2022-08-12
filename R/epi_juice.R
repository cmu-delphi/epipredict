epi_juice <- function(object, ..., composition = "tibble") {
  if (!fully_trained(object)) {
    rlang::abort("At least one step has not been trained. Please run `prep()`.")
  }

  if (!isTRUE(object$retained)) {
    rlang::abort(paste0(
      "Use `retain = TRUE` in `prep()` to be able ",
      "to extract the training set"
    ))
  }

  if (!any(composition == formats)) {
    rlang::abort(paste0(
      "`composition` should be one of: ",
      paste0("'", formats, "'", collapse = ",")
    ))
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(everything())
  }

  # Get user requested columns
  new_data <- object$template
  out_names <- recipes_eval_select(terms, new_data, object$term_info,
                                   check_case_weights = FALSE)
  new_data <- new_data[, out_names]

  ## Since most models require factors, do the conversion from character
  if (!is.null(object$levels)) {
    var_levels <- object$levels
    var_levels <- var_levels[out_names]
    check_values <-
      vapply(var_levels, function(x) {
        (!all(is.na(x)))
      }, c(all = TRUE))
    var_levels <- var_levels[check_values]
    if (length(var_levels) > 0) {
      new_data <- strings2factors(new_data, var_levels)
    }
  }

  if (composition == "dgCMatrix") {
    new_data <- convert_matrix(new_data, sparse = TRUE)
  } else if (composition == "matrix") {
    new_data <- convert_matrix(new_data, sparse = FALSE)
  } else if (composition == "data.frame") {
    new_data <- base::as.data.frame(new_data)
  } else if (composition == "tibble") {
    new_data <- new_data
  }

  new_data
}

environment(epi_juice) <- asNamespace('recipes')
assignInNamespace("juice", epi_juice, ns = "recipes")
