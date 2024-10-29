#' Check that newly created variable names don't overlap
#'
#' `check_pname` is to be used in a slather method to ensure that
#'   newly created variable names don't overlap with existing names.
#'   Throws an warning if check fails, and creates a random string.
#' @param res A data frame or tibble of the newly created variables.
#' @param preds An epi_df or tibble containing predictions.
#' @param object A layer object passed to [slather()].
#' @param newname A string of variable names if the object doesn't contain a
#'   $name element
#'
#' @keywords internal
check_pname <- function(res, preds, object, newname = NULL) {
  if (is.null(newname)) newname <- object$name
  new_preds_names <- colnames(preds)
  intersection <- new_preds_names %in% newname
  if (any(intersection)) {
    newname <- rand_id(newname)
    rlang::warn(
      paste0(
        "Name collision occured in `",
        class(object)[1],
        "`. The following variable names already exists: ",
        paste0(new_preds_names[intersection], collapse = ", "),
        ". Result instead has randomly generated string `",
        newname, "`."
      )
    )
  }
  names(res) <- newname
  res
}

# Copied from `epiprocess`:

#' "Format" a character vector of column/variable names for cli interpolation
#'
#' Designed to give good output if interpolated with cli. Main purpose is to add
#' backticks around variable names when necessary, and something other than an
#' empty string if length 0.
#'
#' @param x `chr`; e.g., `colnames` of some data frame
#' @param empty string; what should be output if `x` is of length 0?
#' @return `chr`
#' @keywords internal
format_varnames <- function(x, empty = "*none*") {
  if (length(x) == 0L) {
    empty
  } else {
    as.character(syms(x))
  }
}

grab_forged_keys <- function(forged, workflow, new_data) {
  # 1. keys in the training data post-prep, based on roles:
  old_keys <- key_colnames(workflow)
  # 2. keys in the test data post-bake, based on roles & structure:
  forged_roles <- forged$extras$roles
  new_key_tbl <- bind_cols(forged_roles$geo_value, forged_roles$key, forged_roles$time_value)
  new_keys <- names(new_key_tbl)
  if (length(new_keys) == 0L) {
    # No epikeytime role assignment; infer from all columns:
    potential_new_keys <- c("geo_value", "time_value")
    forged_tbl <- bind_cols(forged$extras$roles)
    new_keys <- potential_new_keys[potential_new_keys %in% names(forged_tbl)]
    new_key_tbl <- forged_tbl[new_keys]
  }
  # Softly validate:
  if (!(setequal(old_keys, new_keys))) {
    cli_warn(c(
      "Inconsistent epikeytime identifier columns specified/inferred in training vs. in testing data.",
      "i" = "training epikeytime columns, based on roles post-mold/prep: {format_varnames(old_keys)}",
      "i" = "testing epikeytime columns, based on roles post-forge/bake: {format_varnames(new_keys)}",
      "*" = "",
      ">" = 'Some mismatches can be addressed by using `epi_df`s instead of tibbles, or by using `update_role`
             to assign pre-`prep` columns the "geo_value", "key", and "time_value" roles.'
    ))
  }
  # Convert `new_key_tbl` to `epi_df` if not renaming columns nor violating
  # `epi_df` invariants.  Require that our key is a unique key in any case.
  if (all(c("geo_value", "time_value") %in% new_keys)) {
    maybe_as_of <- attr(new_data, "metadata")$as_of # NULL if wasn't epi_df
    try(return(as_epi_df(new_key_tbl, other_keys = new_keys, as_of = maybe_as_of)),
      silent = TRUE
    )
  }
  if (anyDuplicated(new_key_tbl)) {
    duplicate_key_tbl <- new_key_tbl %>% filter(.by = everything(), dplyr::n() > 1L)
    error_part1 <- cli::format_error(
      c(
        "Specified/inferred key columns had repeated combinations in the forged/baked test data.",
        "i" = "Key columns: {format_varnames(new_keys)}",
        "Duplicated keys:"
      )
    )
    error_part2 <- capture.output(print(duplicate_key_tbl))
    rlang::abort(
      paste(collapse = "\n", c(error_part1, error_part2)),
      class = "epipredict__grab_forged_keys__nonunique_key"
    )
  } else {
    return(new_key_tbl)
  }
}

get_parsnip_mode <- function(trainer) {
  if (inherits(trainer, "model_spec")) {
    return(trainer$mode)
  }
  cc <- class(trainer)
  cli_abort(c(
    "`trainer` must be a `parsnip` model.",
    i = "This trainer has class{?s}: {.cls {cc}}."
  ))
}

is_classification <- function(trainer) {
  get_parsnip_mode(trainer) %in% c("classification", "unknown")
}

is_regression <- function(trainer) {
  get_parsnip_mode(trainer) %in% c("regression", "unknown")
}


enlist <- function(...) {
  # converted to thin wrapper around
  rlang::dots_list(
    ...,
    .homonyms = "error",
    .named = TRUE,
    .check_assign = TRUE
  )
}
