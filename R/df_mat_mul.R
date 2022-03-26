#' Multiply columns of a `data.frame` by a matrix
#'
#' @param dat A data.frame
#' @param mat A matrix
#' @param out_names Character vector. Creates the names of the resulting
#'   columns after multiplication. If a scalar, this is treated as a
#'   prefix and the remaining columns will be numbered sequentially.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#'
#' @return A data.frame with the new columns at the right. Original
#'   columns are removed.
#' @export
#'
#' @examples
#' df <- data.frame(matrix(1:200, ncol = 10))
#' mat <- matrix(1:10, ncol = 2)
#' df_mat_mul(df, mat, "z", dplyr::num_range("X", 2:6))
df_mat_mul <- function(dat, mat, out_names = "out", ...) {

  stopifnot(is.matrix(mat), is.data.frame(dat))
  arg_is_chr(out_names)
  if (length(out_names) > 1) stopifnot(length(out_names) == nrow(mat))
  else out_names = paste0(out_names, seq_len(ncol(mat)))

  dat_mat <- dplyr::select(dat, ...)
  nm <- grab_names(dat_mat, everything())
  dat_neg <- dplyr::select(dat, !dplyr::all_of(nm))
  new_cols <- as.matrix(dat_mat) %*% mat
  colnames(new_cols) <- out_names
  dplyr::bind_cols(dat_neg, as.data.frame(new_cols))
}
