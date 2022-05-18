#' Grab any keys associated to an epi_df
#'
#' @param x a data.frame, tibble, or epi_df
#'
#' @return If an `epi_df`, this returns all "keys". Otherwise `NULL`
#' @export
epi_keys <- function(x) {
  keys <- NULL
  if (epiprocess::is_epi_df(x)) {
    keys <- c("time_value", "geo_value",
              attributes(x)$metadata$other_keys)
  }
  keys
}
