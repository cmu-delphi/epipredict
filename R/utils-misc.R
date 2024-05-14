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


grab_forged_keys <- function(forged, workflow, new_data) {
  keys <- c("geo_value", "time_value", "key")
  forged_roles <- names(forged$extras$roles)
  extras <- dplyr::bind_cols(forged$extras$roles[forged_roles %in% keys])
  # 1. these are the keys in the test data after prep/bake
  new_keys <- names(extras)
  # 2. these are the keys in the training data
  old_keys <- key_colnames(workflow)
  # 3. these are the keys in the test data as input
  new_df_keys <- key_colnames(new_data, extra_keys = setdiff(new_keys, keys[1:2]))
  if (!(setequal(old_keys, new_df_keys) && setequal(new_keys, new_df_keys))) {
    cli::cli_warn(paste(
      "Not all epi keys that were present in the training data are available",
      "in `new_data`. Predictions will have only the available keys."
    ))
  }
  if (is_epi_df(new_data)) {
    extras <- as_epi_df(extras)
    attr(extras, "metadata") <- attr(new_data, "metadata")
  } else if (all(keys[1:2] %in% new_keys)) {
    l <- list()
    if (length(new_keys) > 2) l <- list(other_keys = new_keys[-c(1:2)])
    extras <- as_epi_df(extras, additional_metadata = l)
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
