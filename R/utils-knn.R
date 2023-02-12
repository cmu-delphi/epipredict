embedding <- function(dat) {
  dat <- as.matrix(dat)
  dat <- dat / sqrt(rowSums(dat^2) + 1e-12)
  return(dat)
}