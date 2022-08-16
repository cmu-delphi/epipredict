bake.epi_recipe <- function(object, new_data, ..., composition = "tibble") {

  if (rlang::is_missing(new_data)) {
    rlang::abort("'new_data' must be either a data frame or NULL. No value is not allowed.")
  }

  if (is.null(new_data)) {
    return(epi_juice(object, ..., composition = composition))
  }

  if (!fully_trained(object)) {
    rlang::abort("At least one step has not been trained. Please run `prep`.")
  }

  if (!any(composition == formats)) {
    rlang::abort(
      paste0(
        "`composition` should be one of: ",
        paste0("'", formats, "'", collapse = ",")
      )
    )
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(everything())
  }

  # In case someone used the deprecated `newdata`:
  if (is.null(new_data) || is.null(ncol(new_data))) {
    if (any(names(terms) == "newdata")) {
      rlang::abort("Please use `new_data` instead of `newdata` with `bake`.")
    } else {
      rlang::abort("Please pass a data set to `new_data`.")
    }
  }

  if (!is_tibble(new_data)) {
    new_data <- as_tibble(new_data)
  }

  recipes:::check_role_requirements(object, new_data)

  recipes:::check_nominal_type(new_data, object$orig_lvls)

  # Drop completely new columns from `new_data` and reorder columns that do
  # still exist to match the ordering used when training
  original_names <- names(new_data)
  original_training_names <- unique(object$var_info$variable)
  bakeable_names <- intersect(original_training_names, original_names)
  new_data <- new_data[, bakeable_names]

  n_steps <- length(object$steps)

  for (i in seq_len(n_steps)) {
    step <- object$steps[[i]]

    if (recipes:::is_skipable(step)) {
      next
    }

    new_data <- bake(step, new_data = new_data)

    if (!is_tibble(new_data)) {
      abort("bake() methods should always return tibbles")
    }
  }

  # Use `last_term_info`, which maintains info on all columns that got added
  # and removed from the training data. This is important for skipped steps
  # which might have resulted in columns not being added/removed in the test
  # set.
  info <- object$last_term_info

  # Now reduce to only user selected columns
  out_names <- recipes_eval_select(terms, new_data, info,
                                   check_case_weights = FALSE)
  new_data <- new_data[, out_names]

  ## The levels are not null when no nominal data are present or
  ## if strings_as_factors = FALSE in `prep`
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
  }

  new_data
}
