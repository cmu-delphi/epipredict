new_quantiles <- function(q = double(), tau = double()) {
  arg_is_probabilities(tau)

  vec_cast(q, double())
  vec_cast(tau, double())
  stopifnot(length(q) == length(tau))
  stopifnot(!vec_duplicate_any(tau))
  if (is.unsorted(tau)) {
    o <- vec_order(tau)
    q <- q[o]
    tau <- tau[o]
  }
  if (is.unsorted(q)) rlang::abort("q[order(tau)] produces unsorted quantiles.")

  new_rcrd(list(q = q, tau = tau),
           class = c("dist_quantiles", "dist_default"))
}

#' @export
vec_ptype_abbr.dist_quantiles <- function(x, ...) "dist_qntls"
#' @export
vec_ptype_full.dist_quantiles <- function(x, ...) "dist_quantiles"

#' @export
format.dist_quantiles <- function(x, digits = 2, ...) {
  q <- field(x, "q")
  tau <- field(x, "tau")
  rng <- range(tau, na.rm = TRUE)
  paste0("[", round(rng[1], digits), ", ", round(rng[2], digits), "]<q-rng>")
}




#' A distribution parameterized by a set of quantiles
#'
#' @param x A vector of values
#' @param tau A vector of probabilities corresponding to `x`
#'
#' @export
#'
#' @import vctrs
#' @examples
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2,.4,.6,.8)))
#' quantile(dstn, p = c(.1, .25, .5, .9))
#' median(dstn)
#'
#' # it's a bit annoying to inspect the data
#' vctrs::vec_data(vctrs::vec_data(dstn[1])[[1]])
dist_quantiles <- function(x, tau) {
  if (!is.list(x)) x <- list(x)
  if (!is.list(tau)) tau <- list(tau)

  x <- as_list_of(x, .ptype = double())
  tau <- as_list_of(tau, .ptype = double())
  args <- vec_recycle_common(x = x, tau = tau)
  qntls <- as_list_of(map2(args$x, args$tau, new_quantiles))
  new_vctr(qntls, class = "distribution")
}


# format.dist_quantiles <- function(x, ...) {
#   sprintf("quantiles[%s]", length(x))
# }


#' Summarize a distribution with a set of quantiles
#'
#' @param x a `distribution` vector
#' @param p a vector of probabilities at which to calculate quantiles
#' @param ... additional arguments passed on to the `quantile` method
#'
#' @return a `distribution` vector containing `dist_quantiles`
#' @export
#'
#' @examples
#' library(distributional)
#' dstn <- dist_normal(c(10, 2), c(5, 10))
#' extrapolate_quantiles(dstn, p = c(.25, 0.5, .75))
#'
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2,.4,.6,.8)))
#' # because this distribution is already quantiles, any extra quantiles are
#' # appended
#' extrapolate_quantiles(dstn, p = c(.25, 0.5, .75))
#'
#' dstn <- c(dist_normal(c(10, 2), c(5, 10)),
#'   dist_quantiles(list(1:4, 8:11), list(c(.2,.4,.6,.8))))
#' extrapolate_quantiles(dstn, p = c(.25, 0.5, .75))
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
  tau <- field(x, "tau")
  qvals <- field(x, "q")
  new_quantiles(q = c(qvals, q), tau = c(tau, p))
}


#' Turn a a vector of quantile distributions into a list-col
#'
#' @param x a `distribution` containing `dist_quantiles`
#'
#' @return a list-col
#' @export
#'
#' @examples
#' edf <- case_death_rate_subset[1:3,]
#' edf$q <- dist_quantiles(list(1:5, 2:4, 3:10), list(1:5/6, 2:4/5, 3:10/11))
#'
#' edf_nested <- edf %>% dplyr::mutate(q = nested_quantiles(q))
#' edf_nested %>% tidyr::unnest(q)
nested_quantiles <- function(x) {
  stopifnot(is_distribution(x),
            all(stats::family(x) == "quantiles"))
  distributional:::dist_apply(x, .f = function(z) {
    tibble::as_tibble(vec_data(z)) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(), as.double)) %>%
      list_of()
  })
}




#' @export
#' @importFrom stats median qnorm family
median.dist_quantiles <- function(x, ..., middle = c("cubic", "linear")) {
  quantile(x, 0.5, ..., middle = middle)
}

#' @export
#' @importFrom stats quantile
#' @import distributional
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

    tau <- field(x, "tau")
    qvals <- field(x, "q")
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
    if (length(indr) > 0) {
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
  tau <- field(x, "tau")
  qvals <- field(x, "q")
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
  stats::qnorm(target, ms$m, ms$s)
}

