df_mat_mul <- function(dat, mat, out_names = "out", ...) {

  stopifnot(is.matrix(mat), is.data.frame(dat))
  arg_is_chr(out_names)
  if (length(out_names) > 1) stopifnot(length(out_names) == nrow(mat))
  else out_names = paste0(out_names, seq_len(nrow(mat)))

  dat_mat <- dplyr::select(dat, ...)
  nm <- grab_names(dat_mat, everything())
  dat_neg <- dplyr::select(dat, !dplyr::all_of(nm))
  new_cols <- as.matrix(dat_mat) %*% mat
  colnames(new_cols) <- out_names
  bind_cols(dat_neg, as.data.frame(new_cols))
}
