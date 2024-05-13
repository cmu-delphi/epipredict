walk2 <- function(.x, .y, .f, ...) {
  map2(.x, .y, .f, ...)
  invisible(.x)
}

map_vec <- function(.x, .f, ...) {
  out <- map(.x, .f, ...)
  vctrs::list_unchop(out)
}

map_dfr <- function(.x, .f, ..., .id = NULL) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  res <- map(.x, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}

map2_dfr <- function(.x, .y, .f, ..., .id = NULL) {
  .f <- rlang::as_function(.f, env = rlang::global_env())
  res <- map2(.x, .y, .f, ...)
  dplyr::bind_rows(res, .id = .id)
}
