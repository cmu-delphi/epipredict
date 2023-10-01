new_quantiles <- function(values = double(), quantile_values = double()) {
  arg_is_probabilities(quantile_values)

  vec_cast(values, double())
  vec_cast(quantile_values, double())
  stopifnot(length(values) == length(quantile_values))
  stopifnot(!vec_duplicate_any(quantile_values))
  if (is.unsorted(quantile_values)) {
    o <- vec_order(quantile_values)
    values <- values[o]
    quantile_values <- quantile_values[o]
  }
  if (is.unsorted(values, na.rm = TRUE)) {
    cli::cli_abort("`values[order(quantile_values)]` produces unsorted quantiles.")
  }

  new_rcrd(list(values = values, quantile_values = quantile_values),
           class = c("dist_quantiles", "dist_default")
  )
}


#' @export
vec_ptype_abbr.dist_quantiles <- function(x, ...) "dist_qntls"
#' @export
vec_ptype_full.dist_quantiles <- function(x, ...) "dist_quantiles"

#' @export
format.dist_quantiles <- function(x, digits = 2, ...) {
  q <- field(x, "values")
  m <- suppressWarnings(median(x))
  paste0("quantiles(", round(m, digits), ")[", vctrs::vec_size(q), "]")
}


#' A distribution parameterized by a set of quantiles
#'
#' @param values A vector of values
#' @param quantile_values A vector of probabilities corresponding to `values`
#'
#' @export
#'
#' @examples
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' quantile(dstn, p = c(.1, .25, .5, .9))
#' median(dstn)
#'
#' # it's a bit annoying to inspect the data
#' distributional::parameters(dstn[1])
#' nested_quantiles(dstn[1])[[1]]
#'
#' dist_quantiles(1:4, 1:4 / 5)
#' dist_quantiles(1:4, c(1, 3, 2, 4) / 5)
dist_quantiles <- function(values, quantile_values) {
  if (!is.list(values)) values <- list(values)
  if (!is.list(quantile_values)) quantile_values <- list(quantile_values)

  values <- as_list_of(values, .ptype = double())
  quantile_values <- as_list_of(quantile_values, .ptype = double())
  args <- vec_recycle_common(values = values, quantile_values = quantile_values)
  qntls <- as_list_of(map2(args$values, args$quantile_values, new_quantiles))
  new_vctr(qntls, class = "distribution")
}

validate_dist_quantiles <- function(values, quantile_values) {
  map(quantile_values, arg_is_probabilities)
  common_length <- vctrs::vec_size_common( # aborts internally
    values = values,
    quantile_values = quantile_values
  )
  length_diff <- vctrs::list_sizes(values) != vctrs::list_sizes(quantile_values)
  if (any(length_diff)) {
    cli::cli_abort(c(
      "`values` and `quantile_values` must have common length.",
      i = "Mismatches found at position(s): {.val {which(length_diff)}}."
    ))
  }
  tau_duplication <- map_lgl(quantile_values, vctrs::vec_duplicate_any)
  if (any(tau_duplication)) {
    cli::cli_abort(c(
      "`quantile_values` must not be duplicated.",
      i = "Duplicates found at position(s): {.val {which(tau_duplication)}}."
    ))
  }
}


#' Summarize a distribution with a set of quantiles
#'
#' @param x a `distribution` vector
#' @param probs a vector of probabilities at which to calculate quantiles
#' @param ... additional arguments passed on to the `quantile` method
#'
#' @return a `distribution` vector containing `dist_quantiles`
#' @export
#'
#' @examples
#' library(distributional)
#' dstn <- dist_normal(c(10, 2), c(5, 10))
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
#'
#' dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' # because this distribution is already quantiles, any extra quantiles are
#' # appended
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
#'
#' dstn <- c(
#'   dist_normal(c(10, 2), c(5, 10)),
#'   dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
#' )
#' extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
extrapolate_quantiles <- function(x, probs, ...) {
  UseMethod("extrapolate_quantiles")
}

#' @export
extrapolate_quantiles.distribution <- function(x, probs, ...) {
  arg_is_probabilities(probs)
  dstn <- lapply(vec_data(x), extrapolate_quantiles, p = probs, ...)
  distributional:::wrap_dist(dstn)
}

#' @export
extrapolate_quantiles.dist_default <- function(x, probs, ...) {
  q <- quantile(x, probs, ...)
  new_quantiles(values = q, quantile_values = probs)
}

#' @export
extrapolate_quantiles.dist_quantiles <- function(x, probs, ...) {
  q <- quantile(x, probs, ...)
  tau <- field(x, "quantile_values")
  qvals <- field(x, "values")
  new_quantiles(values = c(qvals, q), quantile_values = c(tau, probs))
}

is_dist_quantiles <- function(x) {
  is_distribution(x) & all(stats::family(x) == "quantiles")
}


#' Turn a vector of quantile distributions into a list-col
#'
#' @param x a `distribution` containing `dist_quantiles`
#'
#' @return a list-col
#' @export
#'
#' @examples
#' edf <- case_death_rate_subset[1:3, ]
#' edf$q <- dist_quantiles(list(1:5, 2:4, 3:10), list(1:5 / 6, 2:4 / 5, 3:10 / 11))
#'
#' edf_nested <- edf %>% dplyr::mutate(q = nested_quantiles(q))
#' edf_nested %>% tidyr::unnest(q)
nested_quantiles <- function(x) {
  stopifnot(is_dist_quantiles(x))
  map(
    x,
    ~ distributional::parameters(.x) %>%
      tidyr::unnest(tidyselect::everything())
  )
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
        i = "Check failed for variables(s) {.var {nms}}."
      )
    )
  }
  if (length(cols) > 1L) {
    for (col in cols) {
      .data <- .data %>%
        tidyr::unnest(tidyselect::all_of(col)) %>%
        tidyr::pivot_wider(
          names_from = "quantile_values", values_from = "values",
          names_prefix = paste0(col, "_")
        )
    }
  } else {
    .data <- .data %>%
      tidyr::unnest(tidyselect::all_of(cols)) %>%
      tidyr::pivot_wider(names_from = "quantile_values", values_from = "values")
  }
  .data
}




#' @export
#' @importFrom stats median qnorm family
median.dist_quantiles <- function(x, na.rm = FALSE, ..., middle = c("cubic", "linear")) {
  tau <- field(x, "quantile_values")
  qvals <- field(x, "values")
  if (0.5 %in% tau) return(qvals[match(0.5, tau)])
  if (min(tau) > 0.5 || max(tau) < 0.5 || length(tau) < 2) return(NA)
  if (length(tau) < 3 || min(tau) > .25 || max(tau) < .75) {
   return(stats::approx(tau, qvals, xout = 0.5)$y)
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
quantile.dist_quantiles <- function(
    x, probs, ...,
    middle = c("cubic", "linear"),
    left_tail = c("normal", "exponential"),
    right_tail = c("normal", "exponential")) {
  arg_is_probabilities(probs)
  middle <- match.arg(middle)
  left_tail <- match.arg(left_tail)
  right_tail <- match.arg(right_tail)
  quantile_extrapolate(x, probs, middle, left_tail, right_tail)
}


quantile_extrapolate <- function(x, tau_out, middle, left_tail, right_tail) {
  tau <- field(x, "quantile_values")
  qvals <- field(x, "values")
  r <- range(tau, na.rm = TRUE)
  qvals_out <- rep(NA, length(tau_out))

  # short circuit if we aren't actually extrapolating
  # matches to ~15 decimals
  if (all(tau_out %in% tau)) {
    return(qvals[match(tau_out, tau)])
  }
  if (length(qvals) < 3 || r[1] > .25 || r[2] < .75) {
    cli::cli_warn(c(
      "Quantile extrapolation is not possible with fewer than",
      "3 quantiles or when the probs don't span [.25, .75]"
    ))
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
        qvals_out[indm] <- Q(tau_out[indm])
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
  if (any(indl)) {
    qvals_out[indl] <- tail_extrapolate(
      tau_out[indl], quartiles, "left", left_tail
    )
  }
  if (any(indr)) {
    qvals_out[indr] <- tail_extrapolate(
      tau_out[indr], quartiles, "right", right_tail
    )
  }
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
  if (type == "normal") {
    return(norm_tail_q(p, par, tau_out))
  }
  if (type == "exponential") {
    return(exp_tail_q(p, par, tau_out))
  }
}


exp_q_par <- function(q) {
  # tau should always be c(.75, .5) or c(.25, .5)
  iqr <- 2 * abs(diff(q))
  s <- iqr / (2 * log(2))
  m <- q[2]
  return(list(m = m, s = s))
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
  return(list(m = m, s = s))
}

norm_tail_q <- function(p, q, target) {
  ms <- norm_q_par(q)
  stats::qnorm(target, ms$m, ms$s)
}

#' @method Math dist_quantiles
#' @export
Math.dist_quantiles <- function(x, ...) {
  quantile_values <- field(x, "quantile_values")
  values <- field(x, "values")
  values <- vctrs::vec_math(.Generic, values, ...)
  new_quantiles(values = values, quantile_values = quantile_values)
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
    tau1 <- field(e1, "quantile_values")
  }
  if (is_quantiles[2]) {
    q2 <- field(e2, "values")
    tau2 <- field(e2, "quantile_values")
  }
  tau <- union(tau1, tau2)
  if (all(is_dist)) {
    cli::cli_abort(
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
  new_quantiles(values = q, quantile_values = tau)
}

#' @method is.na distribution
#' @export
is.na.distribution <- function(x) {
  sapply(vctrs::vec_data(x), is.na)
}

#' @method is.na dist_quantiles
#' @export
is.na.dist_quantiles <- function(x) {
  q <- field(x, "values")
  all(is.na(q))
}
