residual_quantiles <- function(r, point, levels, symmetrize) {
  if (is.null(levels)) return(data.frame(point = point))

  s <- ifelse(symmetrize, -1, NA)
  q <- quantile(c(r, s * r), probs = levels, na.rm = TRUE)
  out <- data.frame(point = point, outer(point, q, "+"))
  names(out)[-1] <- sub("\\.", "_", sprintf("q%s", levels * 100))
  out
}
