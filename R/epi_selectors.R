all_epi_keys <- function() {
  union(base_epi_keys(), has_role("key"))
}

base_epi_keys <- function() {
  union(has_role("time_value"), has_role("geo_value"))
}
