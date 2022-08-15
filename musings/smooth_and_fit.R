smooth_and_fit <- function(dat, H, kronecker_version) {
  if (kronecker_version) {
    stop("not yet implemented")
  } else {
    dat <- df_mat_mul(dat, H, "y", starts_with("y"))
    ny <- grab_names(dat, starts_with("y"))
    nx <- grab_names(dat, starts_with("x"))
    form <- stats::as.formula(paste(
      "cbind(", paste(ny, collapse = ","), ") ~ ", # multivariate y
      paste(nx, collapse = "+"), "+ 0"))
    obj <- stats::lm(form, data = dat %>%
                       dplyr::select(starts_with(c("x","y"))))
  }
  return(list(obj = obj, dat = dat))
}
