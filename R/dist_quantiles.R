
# placeholder to avoid errors, but not ideal
#' @export
mean.quantile_pred <- function(x, na.rm = FALSE, ...) {
  median(x, ...)
}

#' @export
#' @importFrom stats quantile
quantile.quantile_pred <- function(x, p, ..., middle = c("cubic", "linear")) {
  arg_is_probabilities(p)
  p <- sort(p)
  middle <- rlang::arg_match(middle)
  quantile_extrapolate(x, p, middle)
}


quantile_extrapolate <- function(x, tau_out, middle) {
  tau <- x %@% "quantile_levels"
  qvals <- as.matrix(x)

  # short circuit if we aren't actually extrapolating
  # matches to ~15 decimals
  if (all(tau_out %in% tau)) {
    return(hardhat::quantile_pred(
      qvals[ ,match(tau_out, tau), drop = FALSE], tau_out
    ))
  }
  if (length(tau) < 2) {
    cli_abort(paste(
      "Quantile extrapolation is not possible when fewer than 2 quantiles",
      "are available."
    ))
  }
  qvals_out <- map(
    vctrs::vec_chop(qvals),
    ~ extrapolate_quantiles_single(.x, tau, tau_out, middle)
  )

  hardhat::quantile_pred(qvals_out, tau_out)
}

extrapolate_quantiles_single <- function(qvals, tau, tau_out, middle) {
  indl <- tau_out < min(tau)
  indr <- tau_out > max(tau)
  indm <- !indl & !indr
  qvals_out <- rep(NA, length(tau_out))

  if (middle == "cubic") {
    method <- "cubic"
    result <- tryCatch({
      Q <- stats::splinefun(tau, qvals, method = "hyman")
      quartiles <- Q(c(.25, .5, .75))
    },
    error = function(e) {
      return(NA)
    })
  }
  if (middle == "linear" || any(is.na(result))) {
    method <- "linear"
    quartiles <- stats::approx(tau, qvals, c(.25, .5, .75))$y
  }
  if (any(indm)) {
    qvals_out[indm] <- switch(
      method,
      linear = stats::approx(tau, qvals, tau_out[indm])$y,
      cubic = Q(tau_out[indm])
    )
  }
  if (any(indl) || any(indr)) {
    qv <- data.frame(
      q = c(tau, tau_out[indm]),
      v = c(qvals, qvals_out[indm])
    ) %>%
      dplyr::distinct(q, .keep_all = TRUE) %>%
      arrange(q)
  }
  if (any(indl)) {
    qvals_out[indl] <- tail_extrapolate(tau_out[indl], utils::head(qv, 2))
  }
  if (any(indr)) {
    qvals_out[indr] <- tail_extrapolate(tau_out[indr], utils::tail(qv, 2))
  }
  qvals_out
}

logit <- function(p) {
  p <- pmax(pmin(p, 1), 0)
  log(p) - log(1 - p)
}

# extrapolates linearly on the logistic scale using
# the two points nearest the tail
tail_extrapolate <- function(tau_out, qv) {
  if (nrow(qv) == 1L) {
    return(rep(qv$v[1], length(tau_out)))
  }
  x <- logit(qv$q)
  x0 <- logit(tau_out)
  y <- qv$v
  m <- diff(y) / diff(x)
  m * (x0 - x[1]) + y[1]
}
