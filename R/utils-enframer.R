enframer <- function(df, x, fill = NA) {
  stopifnot(is.data.frame(df))
  stopifnot(length(fill) == 1 || length(fill) == nrow(df))
  arg_is_chr(x, allow_null = TRUE)
  if (is.null(x)) {
    return(df)
  }
  if (any(names(df) %in% x)) {
    stop("In enframer: some new cols match existing column names")
  }
  for (v in x) df <- dplyr::mutate(df, !!v := fill)
  df
}

enlist <- function(...) {
  # in epiprocess
  x <- list(...)
  n <- as.character(sys.call())[-1]
  if (!is.null(n0 <- names(x))) n[n0 != ""] <- n0[n0 != ""]
  names(x) <- n
  x
}
