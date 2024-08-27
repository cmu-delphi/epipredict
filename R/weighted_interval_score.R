#' Compute weighted interval score
#'
#' Weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param x distribution. A vector of class distribution. Ideally, this vector
#'   contains `dist_quantiles()`, though other distributions are supported when
#'   `quantile_levels` is specified. See below.
#' @param actual double. Actual value(s)
#' @param quantile_levels probabilities. If specified, the score will be
#'   computed at this set of levels.
#' @param ... not used
#'
#' @return a vector of nonnegative scores.
#'
#' @export
#' @examples
#' quantile_levels <- c(.2, .4, .6, .8)
#' predq_1 <- 1:4 #
#' predq_2 <- 8:11
#' dstn <- dist_quantiles(list(predq_1, predq_2), quantile_levels)
#' actual <- c(3.3, 7.1)
#' weighted_interval_score(dstn, actual)
#' weighted_interval_score(dstn, actual, c(.25, .5, .75))
#'
#' library(distributional)
#' dstn <- dist_normal(c(.75, 2))
#' weighted_interval_score(dstn, 1, c(.25, .5, .75))
#'
#' # Missing value behaviours
#' dstn <- dist_quantiles(c(1, 2, NA, 4), 1:4 / 5)
#' weighted_interval_score(dstn, 2.5)
#' weighted_interval_score(dstn, 2.5, 1:9 / 10)
#' weighted_interval_score(dstn, 2.5, 1:9 / 10, na_handling = "drop")
#' weighted_interval_score(dstn, 2.5, na_handling = "propagate")
#' weighted_interval_score(dist_quantiles(1:4, 1:4 / 5), 2.5, 1:9 / 10,
#'   na_handling = "fail"
#' )
#'
#'
#' # Using some actual forecasts --------
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value >= "2021-10-01", time_value <= "2021-12-01")
#' preds <- flatline_forecaster(
#'   jhu, "death_rate",
#'   flatline_args_list(quantile_levels = c(.01, .025, 1:19 / 20, .975, .99))
#' )$predictions
#' actuals <- case_death_rate_subset %>%
#'   filter(time_value == as.Date("2021-12-01") + 7) %>%
#'   select(geo_value, time_value, actual = death_rate)
#' preds <- left_join(preds, actuals,
#'   by = c("target_date" = "time_value", "geo_value")
#' ) %>%
#'   mutate(wis = weighted_interval_score(.pred_distn, actual))
#' preds
weighted_interval_score <- function(x, actual, quantile_levels = NULL, ...) {
  UseMethod("weighted_interval_score")
}

#' @export
weighted_interval_score.default <- function(x, actual,
                                            quantile_levels = NULL, ...) {
  cli_abort(c(
    "Weighted interval score can only be calculated if `x`",
    "has class {.cls distribution}."
  ))
}

#' @export
weighted_interval_score.distribution <- function(
    x, actual,
    quantile_levels = NULL, ...) {
  assert_numeric(actual, finite = TRUE)
  l <- vctrs::vec_recycle_common(x = x, actual = actual)
  map2_dbl(
    .x = vctrs::vec_data(l$x),
    .y = l$actual,
    .f = weighted_interval_score,
    quantile_levels = quantile_levels,
    ...
  )
}

#' @export
weighted_interval_score.dist_default <- function(x, actual,
                                                 quantile_levels = NULL, ...) {
  rlang::check_dots_empty()
  if (is.null(quantile_levels)) {
    cli_warn(c(
      "Weighted interval score isn't implemented for {.cls {class(x)}}",
      "as we don't know what set of quantile levels to use.",
      "Use a {.cls dist_quantiles} or pass `quantile_levels`.",
      "The result for this element will be `NA`."
    ))
    return(NA)
  }
  x <- extrapolate_quantiles(x, probs = quantile_levels)
  weighted_interval_score(x, actual, quantile_levels = NULL)
}

#' @param na_handling character. Determines how `quantile_levels` without a
#'   corresponding `value` are handled. For `"impute"`, missing values will be
#'   calculated if possible using the available quantiles. For `"drop"`,
#'   explicitly missing values are ignored in the calculation of the score, but
#'   implicitly missing values are imputed if possible.
#'   For `"propogate"`, the resulting score will be `NA` if any missing values
#'   exist in the original `quantile_levels`. Finally, if
#'   `quantile_levels` is specified, `"fail"` will result in
#'   the score being `NA` when any required quantile levels (implicit or explicit)
#'   are do not have corresponding values.
#' @describeIn weighted_interval_score Weighted interval score with
#'   `dist_quantiles` allows for different `NA` behaviours.
#' @export
weighted_interval_score.dist_quantiles <- function(
    x, actual,
    quantile_levels = NULL,
    na_handling = c("impute", "drop", "propagate", "fail"),
    ...) {
  rlang::check_dots_empty()
  if (is.na(actual)) {
    return(NA)
  }
  if (all(is.na(vctrs::field(x, "values")))) {
    return(NA)
  }
  na_handling <- rlang::arg_match(na_handling)
  old_quantile_levels <- field(x, "quantile_levels")
  if (na_handling == "fail") {
    if (is.null(quantile_levels)) {
      cli_abort('`na_handling = "fail"` requires `quantile_levels` to be specified.')
    }
    old_values <- field(x, "values")
    if (!all(quantile_levels %in% old_quantile_levels) || any(is.na(old_values))) {
      return(NA)
    }
  }
  tau <- quantile_levels %||% old_quantile_levels
  x <- extrapolate_quantiles(x, probs = tau, replace_na = (na_handling == "impute"))
  q <- field(x, "values")[field(x, "quantile_levels") %in% tau]
  na_rm <- (na_handling == "drop")
  2 * mean(pmax(tau * (actual - q), (1 - tau) * (q - actual)), na.rm = na_rm)
}
