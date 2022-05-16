# These are copied from `recipes`

fun_calls <- function (f) {
  if (is.function(f)) fun_calls(body(f))
  else if (is_quosure(f)) fun_calls(quo_get_expr(f))
  else if (is.call(f)) {
    fname <- as.character(f[[1]])
    if (identical(fname, ".Internal"))
      return(fname)
    unique(c(fname, unlist(lapply(f[-1], fun_calls), use.names = FALSE)))
  }
}

inline_check <- function(x) {
  funs <- fun_calls(x)
  funs <- funs[!(funs %in% c("~", "+", "-"))]
  if (length(funs) > 0) {
    rlang::abort(paste0(
      "No in-line functions should be used here; ",
      "use steps to define baking actions."
    ))
  }
  invisible(x)
}
