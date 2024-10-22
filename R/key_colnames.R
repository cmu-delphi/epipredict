#' @export
key_colnames.recipe <- function(x, ..., exclude = character()) {
  geo_key <- x$var_info$variable[x$var_info$role %in% "geo_value"]
  time_key <- x$var_info$variable[x$var_info$role %in% "time_value"]
  keys <- x$var_info$variable[x$var_info$role %in% "key"]
  full_key <- c(geo_key, keys, time_key) %||% character(0L)
  full_key[!full_key %in% exclude]
}

#' @export
key_colnames.epi_workflow <- function(x, ..., exclude = character()) {
  # safer to look at the mold than the preprocessor
  mold <- hardhat::extract_mold(x)
  molded_names <- names(mold$extras$roles)
  geo_key <- names(mold$extras$roles[molded_names %in% "geo_value"]$geo_value)
  time_key <- names(mold$extras$roles[molded_names %in% "time_value"]$time_value)
  keys <- names(mold$extras$roles[molded_names %in% "key"]$key)
  full_key <- c(geo_key, keys, time_key) %||% character(0L)
  full_key[!full_key %in% exclude]
}

kill_time_value <- function(v) {
  arg_is_chr(v)
  v[v != "time_value"]
}

epi_keys_only <- function(x, ...) {
  kill_time_value(key_colnames(x, ...))
}
