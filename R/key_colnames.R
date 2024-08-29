#' @export
key_colnames.recipe <- function(x, ...) {
  possible_keys <- c("geo_value", "time_value", "key")
  keys <- x$var_info$variable[x$var_info$role %in% possible_keys]
  keys[order(match(keys, possible_keys))] %||% character(0L)
}

#' @export
key_colnames.epi_workflow <- function(x, ...) {
  # safer to look at the mold than the preprocessor
  mold <- hardhat::extract_mold(x)
  possible_keys <- c("geo_value", "time_value", "key")
  molded_names <- names(mold$extras$roles)
  keys <- map(mold$extras$roles[molded_names %in% possible_keys], names)
  keys <- unname(unlist(keys))
  keys[order(match(keys, possible_keys))] %||% character(0L)
}

kill_time_value <- function(v) {
  arg_is_chr(v)
  v[v != "time_value"]
}

epi_keys_only <- function(x, ...) {
  kill_time_value(key_colnames(x, ...))
}
