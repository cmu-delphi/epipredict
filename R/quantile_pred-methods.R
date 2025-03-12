#' A distribution parameterized by a set of quantiles
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. The recommended alternative is
#' [hardhat::quantile_pred()].
#'
#' @param values A vector (or list of vectors) of values.
#' @param quantile_levels A vector (or list of vectors) of probabilities
#'   corresponding to `values`.
#'
#' When creating multiple sets of `values`/`quantile_levels` resulting in
#' different distributions, the sizes must match. See the examples below.
#'
#' @return A vector of class `"distribution"`.
#'
#' @export
#' @keywords internal
#'
#' @importFrom vctrs as_list_of vec_recycle_common new_vctr
dist_quantiles <- function(values, quantile_levels) {
  lifecycle::deprecate_warn("0.1.11", "dist_quantiles()", "hardhat::quantile_pred()")
  if (is.list(values)) {
    n <- length(values)
    values <- unlist(values)
    return(quantile_pred(matrix(values, nrow = n, byrow = TRUE), quantile_levels))
  } else if (is.matrix(values)) {
    return(quantile_pred(values, quantile_levels))
  } else if (is.vector(values)) {
    return(quantile_pred(matrix(values, nrow = 1), quantile_levels))
  }
  cli_abort(c(
    "`dist_quantiles()` is deprecated and the format of `values` could not",
    `!` = "be automatically converted to work with the replacement.",
    i = "See {.fn hardhat::quantile_pred}."
  ))
}

# placeholder to avoid errors, but not ideal
#' @importFrom hardhat quantile_pred
#' @export
mean.quantile_pred <- function(x, na.rm = FALSE, ...) {
  median(x, ...)
}

# These next 3 functions should probably be added via PR to {hardhat}
# Only the third is actually *needed* at the moment.
# The second doesn't work correctly (not sure why), but leaving here for the
# future.
#
# We only export the third.
#
# self-self method, should work only if attr(quantile_levels) are compatible
# #' @importFrom vctrs vec_ptype2 vec_cast
# #' @importFrom hardhat extract_quantile_levels
# #' @export
# #' @keywords internal
# vec_ptype2.quantile_pred.quantile_pred <- function(
#     x, y, ..., x_arg = "", y_arg = "", call = caller_env()
# ) {
#   if (all(extract_quantile_levels(y) %in% extract_quantile_levels(x))) {
#     return(x)
#   }
#   if (all(extract_quantile_levels(x) %in% extract_quantile_levels(y))) {
#     return(y)
#   }
#   vctrs::stop_incompatible_type(
#     x, y, x_arg = x_arg, y_arg = y_arg,
#     details = "`quantile_levels` must be compatible (a superset/subset relation)."
#   )
# }

# currently doesn't work
# #' @export
# vec_cast.quantile_pred.quantile_pred <- function(
#     x, to, ..., x_arg = caller_arg(x), to_arg = caller_arg(to),
#     call = caller_env()
# ) {
#   to_ql <- extract_quantile_levels(to)
#   x_ql <- extract_quantile_levels(x)
#   x_in_to <- x_ql %in% to_ql
#   to_in_x <- to_ql %in% x_ql
#   if (all(x_in_to)) {
#     mat <- matrix(NA, ncol = length(to_ql))
#     mat[ , to_in_x] <- c(as.matrix(x))
#   } else if (all(to_in_x)) {
#     mat <- as.matrix(x)[ , x_in_to, drop = FALSE]
#   } else {
#     vctrs::stop_incompatible_type(
#       x, to, x_arg = x_arg, y_arg = to_arg,
#       details = "`quantile_levels` must be compatible (a superset/subset relation)."
#     )
#   }
#   quantile_pred(mat, to_ql)
# }


# Convert the quantile_pred to a data frame (named with the .quantile_levels)
# This powers vec_proxy_equal (and hence ==, !=, is.na, etc)
# It also powers vec_proxy_compare, so, given matching colnames, these should
# work out of the box.
#
#' @importFrom vctrs vec_proxy_equal
#' @export
vec_proxy_equal.quantile_pred <- function(x, ...) {
  as_tibble(x) %>%
    tidyr::pivot_wider(
      names_from = .quantile_levels,
      values_from = .pred_quantile
    ) %>%
    dplyr::select(-.row)
}


# quantiles by treating quantile_pred like a distribution -----------------


#' Quantiles from a distribution
#'
#' Given a [hardhat::quantile_pred] object, users may wish to compute additional
#' `quantile_levels` that are not part of the object. This function attempts
#' to estimate these quantities under some assumptions. Interior probabilities,
#' those contained within existing probabilities are interpolated in a manner
#' controled by the `middle` argument. Those outside existing probabilities
#' are extrapolated under the assumption that the tails of the distribution
#' decays exponentially. Optionally, one may constrain _all_ quantiles to be
#' within some support (say, `[0, Inf)`).
#'
#' @inheritParams stats::quantile
#' @param ... unused
#' @param lower Scalar. Optional lower bound.
#' @param upper Scalar. Optional upper bound.
#' @param middle Controls how extrapolation to "interior" probabilities is
#'   performed. "cubic" attempts to use [stats::splinefun()] while "linear"
#'   uses [stats::approx()]. The "linear" method is used as a fallback if
#'   "cubic" should fail for some reason.
#'
#' @returns a matrix with one row for each entry in `x` and one column for each
#'   value in `probs`
#' @seealso [extrapolate_quantiles()]
#' @export
#' @importFrom stats quantile
#'
#' @examples
#' qp <- quantile_pred(matrix(1:8, nrow = 2, byrow = TRUE), 1:4 / 5)
#' quantile(qp)
#' quantile(qp, lower = 0)
#' quantile(qp, probs = 0.5)
#' quantile(qp, probs = 1:9 / 10)
quantile.quantile_pred <- function(x,
                                   probs = seq(0, 1, 0.25),
                                   na.rm = FALSE,
                                   lower = -Inf,
                                   upper = Inf,
                                   middle = c("cubic", "linear"),
                                   ...) {
  arg_is_probabilities(probs)
  arg_is_scalar(lower, upper, na.rm)
  arg_is_numeric(lower, upper)
  arg_is_lgl(na.rm)

  if (lower > upper) {
    cli_abort("`lower` must be less than `upper`.")
  }

  if (is.unsorted(probs)) probs <- sort(probs)
  middle <- rlang::arg_match(middle)
  snap(quantile_internal(x, probs, middle), lower, upper)
}


quantile_internal <- function(x, tau_out, middle) {
  tau <- x %@% "quantile_levels"
  qvals <- as.matrix(x)

  # short circuit if we aren't actually extrapolating
  # matches to ~15 decimals
  if (all(tau_out %in% tau) && !anyNA(qvals)) {
    return(qvals[, match(tau_out, tau), drop = FALSE])
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
  if (!any(good)) {
    return(qvals_out)
  }
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
    result <- tryCatch(
      {
        Q <- stats::splinefun(tau, qvals, method = "hyman")
        quartiles <- Q(c(.25, .5, .75))
      },
      error = function(e) {
        return(NA)
      }
    )
  }
  if (middle == "linear" || any(is.na(result))) {
    method <- "linear"
    quartiles <- stats::approx(tau, qvals, c(.25, .5, .75))$y
  }
  if (any(indm)) {
    qvals_out[indm] <- switch(method,
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
