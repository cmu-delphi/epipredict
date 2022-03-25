grab_names <- function(dat, ...) {
  x <- rlang::expr(c(...))
  names(tidyselect::eval_select(x, dat))
}
