has_lvls <- function(info) {
  !vapply(info, function(x) all(is.na(x$values)), c(logic = TRUE))
}

strings2factors <- function(x, info) {
  check_lvls <- has_lvls(info)
  if (!any(check_lvls)) {
    return(x)
  }
  info <- info[check_lvls]
  vars <- names(info)
  info <- info[vars %in% names(x)]
  for (i in seq_along(info)) {
    lcol <- names(info)[i]
    x[, lcol] <-
      factor(as.character(x[[lcol]]),
             levels = info[[i]]$values,
             ordered = info[[i]]$ordered
      )
  }
  x
}
