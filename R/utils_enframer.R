enframer <- function(df, x, fill = NA) {
  stopifnot(is.data.frame(df))
  stopifnot(length(fill) == 1 || length(fill) == nrow(df))
  arg_is_chr(x)
  if (any(names(df) %in% x))
    stop("In enframer: some new cols match existing column names")
  for (v in x) df <- dplyr::mutate(df, !!v := fill)
  df
}
