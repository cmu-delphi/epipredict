#' Compute weighted interval score
#'
#' Weighted interval score (WIS), a well-known quantile-based
#' approximation of the commonly-used continuous ranked probability score
#' (CRPS). WIS is a proper score, and can be thought of as a distributional
#' generalization of absolute error. For example, see [Bracher et
#' al. (2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
#' of COVID-19 forecasting.
#'
#' @param x dist_quantiles. A vector of class [dist_quantiles()].
#' @param actual double. Actual value(s)
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
weighted_interval_score <- function(x, actual, ...) {
  UseMethod("weighted_interval_score")
}

#' @export
weighted_interval_score.default <- function(x, actual, ...) {
  cli_abort("Weighted interval score can only be calculated if `x` has class {.cls distribution}.")
}

#' @export
weighted_interval_score.distribution <- function(x, actual, ...) {
  rlang::check_dots_empty()
  assert_numeric(actual, finite = TRUE)
  l <- vctrs::vec_recycle_common(x = x, actual = actual)
  map2_dbl(vctrs::vec_data(l$x), l$actual, weighted_interval_score)
}

#' @export
weighted_interval_score.dist_default <- function(x, actual, ...) {
  cli_warn(c(
    "Weighted interval score is only meaningful for {.cls dist_quantiles}.",
    "This {.cls distribution} vector contains {.cls {class(x)}}.",
    "The result for this element will be `NA`."
  ))
  return(NA)
}

#' @export
weighted_interval_score.dist_quantiles <- function(x, actual, ...) {
  q <- vctrs::field(x, "values")
  tau <- vctrs::field(x, "quantile_levels")
  2 * mean(pmax(tau * (actual - q), (1 - tau) * (q - actual)), na.rm = TRUE)
}
