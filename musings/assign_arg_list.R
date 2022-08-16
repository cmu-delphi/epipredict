#' Assign argument list to inside an environment
#'
#' This function is similar to `attach()` but without the
#' need to detach. Calling it at the beginning of a forecaster
#' makes all members of the `arg_list` available inside the
#' forecaster with out the ugly `args$member` syntax.
#'
#' @param l List of named arguments.
#' @param env The environment where the args should be assigned.
#'   The default goes into the calling environment.
#'
#' @return Nothing is returned. Called for the side effects.
#' @examples
#' \dontrun{
#'   rm(list = ls())
#'   l <- list(a=1, b=c(12, 10), ff = function() -5)
#'   assign_arg_list(l)
#'   a
#' }
assign_arg_list <- function(l, env = parent.frame()) {
  stopifnot(is.list(l), length((nm <- names(l))) == length(l))
  for (a in seq_along(l)) assign(nm[a], l[[a]], envir = env)
}
