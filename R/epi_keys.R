#' Grab any keys associated to an epi_df
#'
#' @param x a data.frame, tibble, or epi_df
#'
#' @return If an `epi_df`, this returns all "keys". Otherwise `NULL`
#' @export
#'
epi_keys <- function(x) {
  UseMethod("epi_keys")
}

#' @export
epi_keys.default <- function(x) {
  NULL
}

#' @export
epi_keys.epi_df <- function(x) {
  c("time_value", "geo_value", attributes(x)$metadata$other_keys)
}

#' @export
epi_keys.recipe <- function(x) {
  x$var_info$variable[x$var_info$role %in% c("time_value", "geo_value", "key")]
}
