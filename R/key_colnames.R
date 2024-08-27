#' @export
key_colnames.recipe <- function(x, ...) {
  x$var_info$variable[x$var_info$role %in% c("time_value", "geo_value", "key")]
}

#' @export
key_colnames.epi_workflow <- function(x, ...) {
  NextMethod(hardhat::extract_mold(x))
}

# a mold is a list extracted from a fitted workflow, gives info about
# training data. But it doesn't have a class
#' @export
key_colnames.list <- function(x, ...) {
  keys <- c("time_value", "geo_value", "key")
  molded_names <- names(x$extras$roles)
  mold_keys <- map(x$extras$roles[molded_names %in% keys], names)
  unname(unlist(mold_keys)) %||% character(0L)
}

kill_time_value <- function(v) {
  arg_is_chr(v)
  v[v != "time_value"]
}

epi_keys_only <- function(x, ...) {
  kill_time_value(key_colnames(x, ...))
}
