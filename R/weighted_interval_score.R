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
weighted_interval_score <- function(
    x,
    actual,
    quantile_levels = NULL,
    na_handling = c("impute", "drop", "propagate", "fail"),
    ...) {
  UseMethod("weighted_interval_score")
}


#' @export
weighted_interval_score.quantile_pred <- function(
    x, actual,
    quantile_levels = NULL,
    na_handling = c("impute", "drop", "propagate", "fail"),
    ...) {
  rlang::check_dots_empty()
  n <- vctrs::vec_size(x)
  if (length(actual) == 1L) actual <- rep(actual, n)
  assert_numeric(actual, finite = TRUE, len = n)
  assert_numeric(quantile_levels, lower = 0, upper = 1, null.ok = TRUE)
  na_handling <- rlang::arg_match(na_handling)
  old_quantile_levels <- x %@% "quantile_levels"
  if (na_handling == "fail") {
    if (is.null(quantile_levels)) {
      cli_abort('`na_handling = "fail"` requires `quantile_levels` to be specified.')
    }
    if (!all(quantile_levels %in% old_quantile_levels)) {
      return(rep(NA_real_, n))
    }
  }
  tau <- quantile_levels %||% old_quantile_levels
  x <- extrapolate_quantiles(x, tau, replace_na = (na_handling == "impute"))
  x <- as.matrix(x)[, attr(x, "quantile_levels") %in% tau]
  na_rm <- (na_handling == "drop")
  map2_dbl(vctrs::vec_chop(x), actual, ~ wis_one_quantile(.x, tau, .y, na_rm))
}

wis_one_quantile <- function(q, tau, actual, na_rm) {
  2 * mean(pmax(tau * (actual - q), (1 - tau) * (q - actual)), na.rm = na_rm)
}
