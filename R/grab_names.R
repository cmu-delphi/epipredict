#' Get the names from a data frame via tidy select
#'
#' Given a data.frame, use `<tidy-select>` syntax to choose
#' some variables. Return the names of those variables
#'
#' As this is an internal function, no checks are performed.
#'
#' @param dat a data.frame
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#'
#' @export
#' @keywords internal
#' @return a character vector
#' @examples
#' df <- data.frame(a = 1, b = 2, cc = rep(NA, 3))
#' grab_names(df, dplyr::starts_with("c"))
grab_names <- function(dat, ...) {
  x <- rlang::expr(c(...))
  names(tidyselect::eval_select(x, dat))
}
