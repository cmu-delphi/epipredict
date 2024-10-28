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
  # 3. keys in the test data post-bake, based on roles:
  forged_roles <- names(forged$extras$roles)
  extras <- bind_cols(forged$extras$roles[forged_roles %in% c("geo_value", "time_value", "key")])
  new_keys <- names(extras)
  if (length(new_keys) == 0L) {
    # No epikeytime role assignment; infer from all columns:
    potential_keys <- c("geo_value", "time_value")
    new_keys <- potential_keys[potential_keys %in% names(bind_cols(forged$extras$roles))]
  }
  # 2. keys in the test data pre-bake based on data structure + post-bake roles:
  new_df_keys <- key_colnames(new_data, other_keys = setdiff(new_keys, c("geo_value", "time_value")))
  # Softly validate, assuming that no steps change epikeytime role assignments:
  if (!(setequal(old_keys, new_df_keys) && setequal(new_df_keys, new_keys))) {
    cli_warn(c(
      "Inconsistent epikeytime identifier columns specified/inferred.",
      "i" = "training epikeytime columns, based on roles post-mold/prep: {format_varnames(old_keys)}",
      "i" = " testing epikeytime columns, based on data structure pre-bake and roles post-forge/bake: {format_varnames(new_df_keys)}",
      "i" = " testing epikeytime columns, based on roles post-forge/bake: {format_varnames(new_keys)}",
      "*" = "Keys will be set giving preference to test-time `epi_df` metadata followed by test-time
             post-bake role settings.",
      ">" = 'Some mismatches can be addressed by using `epi_df`s instead of tibbles, or by using `update_role`
             to assign pre-`prep` columns the "geo_value", "key", and "time_value" roles.'
    ))
  }
  if (is_epi_df(new_data)) {
    # Inference based on test data pre-bake data structure "wins":
    meta <- attr(new_data, "metadata")
    extras <- as_epi_df(extras, as_of = meta$as_of, other_keys = meta$other_keys)
  } else if (all(c("geo_value", "time_value") %in% new_keys)) {
    # Inference based on test data post-bake roles "wins":
    other_keys <- new_keys[!new_keys %in% c("geo_value", "time_value")]
    extras <- as_epi_df(extras, other_keys = other_keys)
  }
  extras
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
