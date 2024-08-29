#' @export
key_colnames.recipe <- function(x, ...) {
  x$var_info$variable[x$var_info$role %in% c("geo_value", "time_value", "key")]
}

#' @export
key_colnames.epi_workflow <- function(x, ...) {
  # safer to look at the mold than the preprocessor
  mold <- hardhat::extract_mold(x)
  keys <- c("geo_value", "time_value", "key")
  molded_names <- names(mold$extras$roles)
  mold_keys <- map(mold$extras$roles[molded_names %in% keys], names)
  unname(unlist(mold_keys)) %||% character(0L)
}

kill_time_value <- function(v) {
  arg_is_chr(v)
  v[v != "time_value"]
}

epi_keys_only <- function(x, ...) {
  kill_time_value(key_colnames(x, ...))
}
