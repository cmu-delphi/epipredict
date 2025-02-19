
# placeholder to avoid errors, but not ideal
#' @importFrom hardhat quantile_pred
#' @export
mean.quantile_pred <- function(x, na.rm = FALSE, ...) {
  median(x, ...)
}


# quantiles by treating quantile_pred like a distribution -----------------


#' @export
#' @importFrom stats quantile
quantile.quantile_pred <- function(x, p, na.rm = FALSE, ...,
                                   middle = c("cubic", "linear")) {
  arg_is_probabilities(p)
  p <- sort(p)
  middle <- rlang::arg_match(middle)
  quantile_internal(x, p, middle)
}


quantile_internal <- function(x, tau_out, middle) {
  tau <- x %@% "quantile_levels"
  qvals <- as.matrix(x)

  # short circuit if we aren't actually extrapolating
  # matches to ~15 decimals
  if (all(tau_out %in% tau) && !anyNA(qvals)) {
    return(qvals[ , match(tau_out, tau), drop = FALSE])
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
  qvals_out <- do.call(rbind, qvals_out) # ensure a matrix of the proper dims
  qvals_out
}

extrapolate_quantiles_single <- function(qvals, tau, tau_out, middle) {
  qvals_out <- rep(NA, length(tau_out))
  good <- !is.na(qvals)
  if (!any(good)) return(qvals_out)
  qvals <- qvals[good]
  tau <- tau[good]

  # in case we only have one point, and it matches something we wanted
  if (length(good) < 2) {
    matched_one <- tau_out %in% tau
    qvals_out[matched_one] <- qvals[matched_one]
    return(qvals_out)
  }

  indl <- tau_out < min(tau)
  indr <- tau_out > max(tau)
  indm <- !indl & !indr

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


# mathematical operations on the values -----------------------------------


#' @importFrom vctrs vec_math
#' @export
#' @method vec_math quantile_pred
vec_math.quantile_pred <- function(.fn, .x, ...) {
  fn <- .fn
  .fn <- getExportedValue("base", .fn)
  if (fn %in% c("any", "all", "prod", "sum", "cumsum", "cummax", "cummin", "cumprod")) {
    cli_abort("{.fn {fn}} is not a supported operation for {.cls quantile_pred}.")
  }
  quantile_levels <- .x %@% "quantile_levels"
  .x <- as.matrix(.x)
  quantile_pred(.fn(.x), quantile_levels)
}

#' @importFrom vctrs vec_arith vec_arith.numeric
#' @export
#' @method vec_arith quantile_pred
vec_arith.quantile_pred <- function(op, x, y, ...) {
  UseMethod("vec_arith.quantile_pred", y)
}

#' @export
#' @method vec_arith.quantile_pred numeric
vec_arith.quantile_pred.numeric <- function(op, x, y, ...) {
  op_fn <- getExportedValue("base", op)
  l <- vctrs::vec_recycle_common(x = x, y = y)
  out <- op_fn(as.matrix(l$x), l$y)
  quantile_pred(out, x %@% "quantile_levels")
}

#' @export
#' @method vec_arith.numeric quantile_pred
vec_arith.numeric.quantile_pred <- function(op, x, y, ...) {
  op_fn <- getExportedValue("base", op)
  l <- vctrs::vec_recycle_common(x = x, y = y)
  out <- op_fn(l$x, as.matrix(l$y))
  quantile_pred(out, y %@% "quantile_levels")
}
