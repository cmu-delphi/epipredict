new_quantiles <- function(q = double(), tau = double()) {
  arg_is_probabilities(tau)

  vctrs::vec_cast(q, double())
  vctrs::vec_cast(tau, double())
  stopifnot(length(q) == length(tau))
  stopifnot(!vctrs::vec_duplicate_any(tau))
  if (is.unsorted(tau)) {
    o <- vctrs::vec_order(tau)
    q <- q[o]
    tau <- tau[o]
  }
  if (is.unsorted(q)) rlang::abort("q[order(tau)] produces unsorted quantiles.")

  vctrs::new_rcrd(list(q = q, tau = tau),
                  class = c("dist_quantiles", "dist_default"))
}

#' @export
vec_ptype_abbr.dist_quantiles <- function(x, ...) "dist_qntls"
#' @export
vec_ptype_full.dist_quantiles <- function(x, ...) "dist_quantiles"

#' @export
format.dist_quantiles <- function(x, digits = 2, ...) {
  q <- vctrs::field(x, "q")
  tau <- vctrs::field(x, "tau")
  rng <- range(tau, na.rm = TRUE)
  paste0("[", round(rng[1], digits), ", ", round(rng[2], digits), "]<q-rng>")
}



#' @importFrom vctrs as_list_of
dist_quantiles <- function(x, tau) {
  if (!is.list(x)) x <- list(x)
  if (!is.list(tau)) tau <- list(tau)

  x <- as_list_of(x, .ptype = double())
  tau <- as_list_of(tau, .ptype = double())
  args <- vctrs::vec_recycle_common(x = x, tau = tau)
  qntls <- as_list_of(map2(args$x, args$tau, new_quantiles))
  vctrs::new_vctr(qntls, class = "distribution")
}


# format.dist_quantiles <- function(x, ...) {
#   sprintf("quantiles[%s]", length(x))
# }

#' @export
extrapolate_quantiles <- function(x, p, ...) {
  UseMethod("extrapolate_quantiles")
}

#' @export
extrapolate_quantiles.distribution <- function(x, p, ...) {
  arg_is_probabilities(p)
  p <- distributional:::arg_listable(p, .ptype = double())
  dstn <- distributional:::dist_apply(x, extrapolate_quantiles, p = p, ...)
  distributional:::wrap_dist(dstn)
}

#' @export
extrapolate_quantiles.dist_default <- function(x, p, ...) {
  q <- quantile(x, p, ...)
  new_quantiles(q = q, tau = p)
}

#' @export
extrapolate_quantiles.dist_quantiles <- function(x, p, ...) {
  q <- quantile(x, p, ...)
  tau <- vctrs::field(x, "tau")
  qvals <- vctrs::field(x, "q")
  new_quantiles(q = c(qvals, q), tau = c(tau, p))
}

#' @export
nested_quantiles <- function(x) {
  stopifnot(is_distribution(x), all(family(x) == "quantiles"))
  distributional:::dist_apply(x, .f = function(z) {
    tibble::as_tibble(vctrs::vec_data(z)) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(), as.double)) %>%
      list_of()
  })
}




#' @export
median.dist_quantiles <- function(x, ..., middle = c("cubic", "linear")) {
  quantile(x, 0.5, ..., middle = middle)
}

#' @export
quantile.dist_quantiles <- function(x, p, ...,
                                    middle = c("cubic", "linear"),
                                    left_tail = c("normal", "exponential"),
                                    right_tail = c("normal", "exponential")) {
  arg_is_probabilities(p)
  middle = match.arg(middle)
  left_tail = match.arg(left_tail)
  right_tail = match.arg(right_tail)
  quantile_extrapolate(x, p, middle, left_tail, right_tail)
}


quantile_extrapolate <- function(x, tau_out, middle, left_tail, right_tail) {

    tau <- vctrs::field(x, "tau")
    qvals <- vctrs::field(x, "q")
    r <- range(tau, na.rm = TRUE)
    qvals_out <- rep(NA, length(tau_out))

    if (length(qvals) < 3 || r[1] > .25 || r[2] < .75) {
      warning(paste("Quantile extrapolation is not possible with fewer than",
                    "3 quantiles or when the probs don't span [.25, .75]"))
      return(qvals_out)
    }

    indl <- which(tau_out < min(tau))
    indr <- which(tau_out > max(tau))
    indm <- which(tau_out >= min(tau) & tau_out <= max(tau))

    if (length(indm) > 0) {
      result = NA # This will get set to 1 if cubic method fails
      if (middle == "cubic") {
        result = tryCatch({
          Q = stats::splinefun(tau, qvals, method="hyman")
          qvals_out[indm] = Q(tau_out[indm])},
          error = function(e) { return(NA) }
        )
      }
      if (middle == "linear" || any(is.na(result))) {
        # Fit quantile function via linear interpolation
        qvals_out[indm] = stats::approx(tau, qvals, tau_out[indm])$y
      }
    }
    if (length(indl) > 0) {
      qvals_out[indl] <- tail_extrapolate(tau_out[indl], Q, "left", left_tail)
    }
    if (length(indr) > 0) {d
      qvals_out[indr] <- tail_extrapolate(tau_out[indr], Q, "right", right_tail)
    }

    qvals_out
  }

tail_extrapolate <- function(tau_out, Qfun, type, tail) {
  p <- c(.25, .5)
  if (tail == "right") p <- 1 - p
  par <- Qfun(p)
  if (tail == "exponential") {
    out = exp_tail_q(p, par, tau_out)
  } else {
    out = norm_tail_q(p, par, tau_out)
  }
  out
}

mean.dist_quantiles <- function(x, ...,
                                left_tail = c("normal", "exponential"),
                                right_tail = c("normal", "exponential")) {
  # TODO: currently nonfunctional
  tau <- vctrs::field(x, "tau")
  qvals <- vctrs::field(x, "q")
  r <- range(tau, na.rm = TRUE)

  if (length(qvals) < 3 || r[1] > .25 || r[2] < .75) {
    warning(paste("Mean approximation is not advised with fewer than",
                  "3 quantiles or when the probs don't span [.25, .75]"))
    return(NA)
  }

  # trapz <-


}

exp_q_par <- function(q) {
  # tau should always be c(.75, .5) or c(.25, .5)
  iqr <- 2 * abs(diff(q))
  s <- iqr / (2*log(2))
  m <- q[2]
  return(list(m=m, s=s))
}

exp_tail_q <- function(p, q, target) {
  ms <- exp_q_par(q)
  qlaplace(target, ms$m, ms$s)
}

qlaplace <- function(p, centre = 0, b = 1) {
  # lower.tail = TRUE, log.p = FALSE
  centre - b * sign(p - 0.5) * log(1 - 2 * abs(p - 0.5))
}

norm_q_par <- function(q) {
  # tau should always be c(.75, .5) or c(.25, .5)
  iqr <- 2 * abs(diff(q))
  s <- iqr / 1.34897950039 # abs(diff(qnorm(c(.75, .25))))
  m <- q[2]
  return(list(m=m, s=s))
}

norm_tail_q <- function(p, q, target) {
  ms <- norm_q_par(q)
  qnorm(target, ms$m, ms$s)
}

