residual_quantiles_with_S3 <- function(obj, dat, point, levels, symmetrize) {
  if (is.null(levels)) return(data.frame(point = point))

  if (any(as.vector(methods(class = class(obj)))) == "residuals") {
    r <- residuals(obj)
  } else {
    r <- dat$y - predict(obj, newdata = dat)
  }
  s <- ifelse(symmetrize, -1, NA)
  q <- quantile(c(r, s * r), probs = levels, na.rm = TRUE)
  out <- data.frame(point=point, outer(point, q, "+"))
  names(out)[-1] <- sub("\\.", "_", sprintf("q%s", levels*100))
  out
}
