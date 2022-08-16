residual_quantiles <- function(r, point, levels, symmetrize) {
  if (is.null(levels)) return(data.frame(point = point))

  s <- ifelse(symmetrize, -1, NA)
  q <- quantile(c(r, s * r), probs = levels, na.rm = TRUE)
  out <- data.frame(point = point, outer(point, q, "+"))
  names(out)[-1] <- probs_to_string(levels)
  out
}


residual_quantiles_normlized <- function(r, point, levels, symmetrize) {
  # use relative rediduals for sampling
  # this will help the performance for residuals with different magnitudes
  if (is.null(levels)) return(data.frame(point = point))
  s <- ifelse(symmetrize, -1, NA)
  q <- quantile(c(r, s * r), probs = levels, na.rm = TRUE)
  out <- data.frame(point = point, outer(point, 1 + q, "*"))
  names(out)[-1] <- probs_to_string(levels)
  out
}