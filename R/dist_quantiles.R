#' @importFrom vctrs field vec_cast new_rcrd
new_quantiles <- function(values = double(1), quantile_levels = double(1)) {
  arg_is_probabilities(quantile_levels)

  vec_cast(values, double())
  vec_cast(quantile_levels, double())
  values <- unname(values)
  if (length(values) == 0L) {
    return(new_rcrd(
      list(
        values = rep(NA_real_, length(quantile_levels)),
        quantile_levels = quantile_levels
      ),
      class = c("dist_quantiles", "dist_default")
    ))
  }
  stopifnot(length(values) == length(quantile_levels))

  stopifnot(!vctrs::vec_duplicate_any(quantile_levels))
  if (is.unsorted(quantile_levels)) {
    o <- vctrs::vec_order(quantile_levels)
    values <- values[o]
    quantile_levels <- quantile_levels[o]
  }
  if (is.unsorted(values, na.rm = TRUE)) {
    cli_abort("`values[order(quantile_levels)]` produces unsorted quantiles.")
  }

  new_rcrd(list(values = values, quantile_levels = quantile_levels),
    class = c("dist_quantiles", "dist_default")
  )
}



#' @importFrom vctrs vec_ptype_abbr vec_ptype_full
#' @export
vec_ptype_abbr.dist_quantiles <- function(x, ...) "dist_qntls"
#' @export
vec_ptype_full.dist_quantiles <- function(x, ...) "dist_quantiles"

#' @export
format.dist_quantiles <- function(x, digits = 2, ...) {
  m <- suppressWarnings(median(x))
  paste0("quantiles(", round(m, digits), ")[", vctrs::vec_size(x), "]")
}


#' A distribution parameterized by a set of quantiles
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
#'
#' @examples
#' dist_quantiles(1:4, 1:4 / 5)
#' dist_quantiles(list(1:3, 1:4), list(1:3 / 4, 1:4 / 5))
#' dstn <- dist_quantiles(list(1:4, 8:11), c(.2, .4, .6, .8))
#' dstn
#'
#' quantile(dstn, p = c(.1, .25, .5, .9))
#' median(dstn)
#'
#' # it's a bit annoying to inspect the data
#' distributional::parameters(dstn[1])
#' nested_quantiles(dstn[1])[[1]]
#'
#' @importFrom vctrs as_list_of vec_recycle_common new_vctr
dist_quantiles <- function(values, quantile_levels) {
  if (!is.list(quantile_levels)) {
    assert_numeric(quantile_levels, lower = 0, upper = 1, any.missing = FALSE, min.len = 1L)
    quantile_levels <- list(quantile_levels)
  }
  if (!is.list(values)) {
    if (length(values) == 0L) values <- NA_real_
    values <- list(values)
  }

  values <- as_list_of(values, .ptype = double())
  quantile_levels <- as_list_of(quantile_levels, .ptype = double())
  args <- vec_recycle_common(values = values, quantile_levels = quantile_levels)

  qntls <- as_list_of(
    map2(args$values, args$quantile_levels, new_quantiles),
    .ptype = new_quantiles(NA_real_, 0.5)
  )
  new_vctr(qntls, class = "distribution")
}

validate_dist_quantiles <- function(values, quantile_levels) {
  map(quantile_levels, arg_is_probabilities)
  common_length <- vctrs::vec_size_common( # aborts internally
    values = values,
    quantile_levels = quantile_levels
  )
  length_diff <- vctrs::list_sizes(values) != vctrs::list_sizes(quantile_levels)
  if (any(length_diff)) {
    cli_abort(c(
      "`values` and `quantile_levels` must have common length.",
      i = "Mismatches found at position(s): {.val {which(length_diff)}}."
    ))
  }
  level_duplication <- map_lgl(quantile_levels, vctrs::vec_duplicate_any)
  if (any(level_duplication)) {
    cli_abort(c(
      "`quantile_levels` must not be duplicated.",
      i = "Duplicates found at position(s): {.val {which(level_duplication)}}."
    ))
  }
}


is_dist_quantiles <- function(x) {
  is_distribution(x) & all(stats::family(x) == "quantiles")
}



#' @export
#' @importFrom stats median qnorm family
median.dist_quantiles <- function(x, na.rm = FALSE, ..., middle = c("cubic", "linear")) {
  quantile_levels <- field(x, "quantile_levels")
  values <- field(x, "values")
  if (0.5 %in% quantile_levels) {
    return(values[match(0.5, quantile_levels)])
  }
  if (length(quantile_levels) < 2 || min(quantile_levels) > 0.5 || max(quantile_levels) < 0.5) {
    return(NA)
  }
  if (length(quantile_levels) < 3 || min(quantile_levels) > .25 || max(quantile_levels) < .75) {
    return(stats::approx(quantile_levels, values, xout = 0.5)$y)
  }
  quantile(x, 0.5, ..., middle = middle)
}

# placeholder to avoid errors, but not ideal
#' @export
mean.dist_quantiles <- function(x, na.rm = FALSE, ..., middle = c("cubic", "linear")) {
  median(x, ..., middle = middle)
}

#' @export
#' @importFrom stats quantile
#' @import distributional
quantile.dist_quantiles <- function(x, p, ..., middle = c("cubic", "linear")) {
  arg_is_probabilities(p)
  p <- sort(p)
  middle <- match.arg(middle)
  quantile_extrapolate(x, p, middle)
}


quantile_extrapolate <- function(x, tau_out, middle) {
  tau <- field(x, "quantile_levels")
  qvals <- field(x, "values")
  nas <- is.na(qvals)
  qvals_out <- rep(NA, length(tau_out))
  qvals <- qvals[!nas]
  tau <- tau[!nas]

  # short circuit if we aren't actually extrapolating
  # matches to ~15 decimals
  if (all(tau_out %in% tau)) {
    return(qvals[match(tau_out, tau)])
  }
  if (length(tau) < 2) {
    cli_abort(
      "Quantile extrapolation is not possible with fewer than 2 quantiles."
    )
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


#' @method Math dist_quantiles
#' @export
Math.dist_quantiles <- function(x, ...) {
  quantile_levels <- field(x, "quantile_levels")
  values <- field(x, "values")
  values <- vctrs::vec_math(.Generic, values, ...)
  new_quantiles(values = values, quantile_levels = quantile_levels)
}

#' @method Ops dist_quantiles
#' @export
Ops.dist_quantiles <- function(e1, e2) {
  is_quantiles <- c(
    inherits(e1, "dist_quantiles"),
    inherits(e2, "dist_quantiles")
  )
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  tau1 <- tau2 <- NULL
  if (is_quantiles[1]) {
    q1 <- field(e1, "values")
    tau1 <- field(e1, "quantile_levels")
  }
  if (is_quantiles[2]) {
    q2 <- field(e2, "values")
    tau2 <- field(e2, "quantile_levels")
  }
  tau <- union(tau1, tau2)
  if (all(is_dist)) {
    cli_abort(
      "You can't perform arithmetic between two distributions like this."
    )
  } else {
    if (is_quantiles[1]) {
      q2 <- e2
    } else {
      q1 <- e1
    }
  }
  q <- vctrs::vec_arith(.Generic, q1, q2)
  new_quantiles(values = q, quantile_levels = tau)
}

#' @method is.na distribution
#' @export
is.na.distribution <- function(x) {
  sapply(vec_data(x), is.na)
}

#' @method is.na dist_quantiles
#' @export
is.na.dist_quantiles <- function(x) {
  q <- field(x, "values")
  all(is.na(q))
}
