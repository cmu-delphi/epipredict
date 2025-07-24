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
  molded_roles <- mold$extras$roles
  extras <- bind_cols(molded_roles$geo_value, molded_roles$key, molded_roles$time_value)
  full_key <- names(extras)
  if (length(full_key) == 0L) {
    # No epikeytime role assignment; infer from all columns:
    potential_keys <- c("geo_value", "time_value")
    full_key <- potential_keys[potential_keys %in% names(bind_cols(molded_roles))]
  }
  full_key[!full_key %in% exclude]
}

kill_time_value <- function(v) {
  arg_is_chr(v)
  v[v != "time_value"]
}

epi_keys_only <- function(x, ...) {
  kill_time_value(key_colnames(x, ...))
}
