#' Bake an epi_recipe
#'
#' @param object A trained object such as a [recipe()] with at least
#'   one preprocessing operation.
#' @param new_data An `epi_df`, data frame or tibble for whom the
#'   preprocessing will be applied. If `NULL` is given to `new_data`,
#'   the pre-processed _training data_ will be returned.
#' @param ... One or more selector functions to choose which variables will be
#'   returned by the function. See [recipes::selections()] for
#'   more details. If no selectors are given, the default is to
#'   use [tidyselect::everything()].
#' @return An `epi_df` that may have different columns than the
#' original columns in `new_data`.
#' @importFrom rlang is_empty quos
#' @importFrom tibble is_tibble as_tibble
#' @importFrom methods is
#' @rdname bake
#' @export
bake.epi_recipe <- function(object, new_data, ...) {
  if (rlang::is_missing(new_data)) {
    rlang::abort("'new_data' must be either an epi_df or NULL. No value is not allowed.")
  }

  if (is.null(new_data)) {
    return(epi_juice(object, ...))
  }

  if (!fully_trained(object)) {
    rlang::abort("At least one step has not been trained. Please run `prep`.")
  }

  terms <- quos(...)
  if (is_empty(terms)) {
    terms <- quos(tidyselect::everything())
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
    check_case_weights = FALSE
  )
  new_data <- new_data[, out_names]

  # The levels are not null when no nominal data are present or
  # if strings_as_factors = FALSE in `prep`
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
