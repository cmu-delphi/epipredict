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
  if (is.unsorted(q, na.rm = TRUE))
    rlang::abort("`q[order(tau)]` produces unsorted quantiles.")

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
  dstn <- lapply(vec_data(x), extrapolate_quantiles, p = p, ...)
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

is_dist_quantiles <- function(x) {
  is_distribution(x) && all(stats::family(x) == "quantiles")
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
  stopifnot(is_dist_quantiles(x))
  distributional:::dist_apply(x, .f = function(z) {
    tibble::as_tibble(vec_data(z)) %>%
      dplyr::mutate(dplyr::across(tidyselect::everything(), as.double)) %>%
      list_of()
  })
}


#' Pivot columns containing `dist_quantile` wider
#'
#' Any selected columns that contain `dist_quantiles` will be "widened" with
#' the "taus" (quantile) serving as names and the values in the data frame.
#' When pivoting multiple columns, the original column name will be used as
#' a prefix.
#'
#' @param .data A data frame, or a data frame extension such as a tibble or
#'   epi_df.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables. Any selected columns should
#'
#' @return An object of the same class as `.data`
#' @export
#'
#' @examples
#' d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 1:3 / 4))
#' d2 <- c(dist_quantiles(2:4, 2:4 / 5), dist_quantiles(3:5, 2:4 / 5))
#' tib <- tibble::tibble(g = c("a", "b"), d1 = d1, d2 = d2)
#'
#' pivot_quantiles(tib, c("d1", "d2"))
#' pivot_quantiles(tib, tidyselect::starts_with("d"))
#' pivot_quantiles(tib, d2)
pivot_quantiles <- function(.data, ...) {
  expr <- rlang::expr(c(...))
  cols <- names(tidyselect::eval_select(expr, .data))
  dqs <- map_lgl(cols, ~ is_dist_quantiles(.data[[.x]]))
  if (!all(dqs)) {
    nms <- cols[!dqs]
    cli::cli_abort(
      "Variables(s) {.var {nms}} are not `dist_quantiles`. Cannot pivot them."
    )
  }
  .data <- .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(cols), nested_quantiles))
  checks <- map_lgl(cols, ~ diff(range(vctrs::list_sizes(.data[[.x]]))) == 0L)
  if (!all(checks)) {
    nms <- cols[!checks]
    cli::cli_abort(
      c("Quantiles must be the same length and have the same set of taus.",
        i = "Check failed for variables(s) {.var {nms}}."))
  }
  if (length(cols) > 1L) {
    for (col in cols) {
      .data <- .data %>%
        tidyr::unnest(tidyselect::all_of(col)) %>%
        tidyr::pivot_wider(
          names_from = "tau", values_from = "q",
          names_prefix = paste0(col, "_")
        )
    }
  } else {
    .data <- .data %>%
      tidyr::unnest(tidyselect::all_of(cols)) %>%
      tidyr::pivot_wider(names_from = "tau", values_from = "q")
  }
  .data
}




#' @export
#' @importFrom stats median qnorm family
median.dist_quantiles <- function(x, ..., middle = c("cubic", "linear")) {
  quantile(x, 0.5, ..., middle = middle)
}

# placeholder to avoid errors, but not ideal
#' @export
mean.dist_quantiles <- function(x, ..., middle = c("cubic", "linear")) {
  median(x, ..., middle = middle)
}

#' @export
#' @importFrom stats quantile
#' @import distributional
quantile.dist_quantiles <- function(x, probs, ...,
                                    middle = c("cubic", "linear"),
                                    left_tail = c("normal", "exponential"),
                                    right_tail = c("normal", "exponential")) {
  arg_is_probabilities(probs)
  middle = match.arg(middle)
  left_tail = match.arg(left_tail)
  right_tail = match.arg(right_tail)
  quantile_extrapolate(x, probs, middle, left_tail, right_tail)
}


quantile_extrapolate <- function(x, tau_out, middle, left_tail, right_tail) {

  tau <- field(x, "tau")
  qvals <- field(x, "q")
  r <- range(tau, na.rm = TRUE)
  qvals_out <- rep(NA, length(tau_out))

  # short circuit if we aren't actually extrapolating
  # matches to ~15 decimals
  if (all(tau_out %in% tau)) return(qvals[match(tau_out, tau)])
  if (length(qvals) < 3 || r[1] > .25 || r[2] < .75) {
    rlang::warn(c("Quantile extrapolation is not possible with fewer than",
                  "3 quantiles or when the probs don't span [.25, .75]"))
    return(qvals_out)
  }

  indl <- tau_out < min(tau)
  indr <- tau_out > max(tau)
  indm <- !indl & !indr

  if (middle == "cubic") {
    method <- "cubic"
    result <- tryCatch({
      Q <- stats::splinefun(tau, qvals, method = "hyman")
      qvals_out[indm] <- Q(tau_out[indm])
      quartiles <- Q(c(.25, .5, .75))},
      error = function(e) { return(NA) }
    )
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
    )}
  if (any(indl)) {
    qvals_out[indl] <- tail_extrapolate(
      tau_out[indl], quartiles, "left", left_tail
    )}
  if (any(indr)) {
      qvals_out[indr] <- tail_extrapolate(
        tau_out[indr], quartiles, "right", right_tail
      )}
  qvals_out
}

tail_extrapolate <- function(tau_out, quartiles, tail, type) {
  if (tail == "left") {
    p <- c(.25, .5)
    par <- quartiles[1:2]
  }
  if (tail == "right") {
    p <- c(.75, .5)
    par <- quartiles[3:2]
  }
  if (type == "normal") return(norm_tail_q(p, par, tau_out))
  if (type == "exponential") return(exp_tail_q(p, par, tau_out))
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

#' @method Math dist_quantiles
#' @export
Math.dist_quantiles <- function(x, ...) {
  tau <- field(x, "tau")
  q <- field(x, "q")
  q <- vctrs::vec_math(.Generic, q, ...)
  new_quantiles(q = q, tau = tau)
}

#' @method Ops dist_quantiles
#' @export
Ops.dist_quantiles <- function(e1, e2) {
  is_quantiles <- c(inherits(e1, "dist_quantiles"),
                    inherits(e2, "dist_quantiles"))
  is_dist <- c(inherits(e1, "dist_default"), inherits(e2, "dist_default"))
  tau1 <- tau2 <- NULL
  if (is_quantiles[1]) {
    q1 <- field(e1, "q")
    tau1 <- field(e1, "tau")
  }
  if (is_quantiles[2]) {
    q2 <- field(e2, "q")
    tau2 <- field(e2, "tau")
  }
  tau <- union(tau1, tau2)
  if (all(is_dist)) {
    rlang::abort(
      "You can't perform arithmetic between two distributions like this."
    )
  } else {
    if (is_quantiles[1]) q2 <- e2
    else q1 <- e1
  }
  q <- vctrs::vec_arith(.Generic, q1, q2)
  new_quantiles(q = q, tau = tau)
}

