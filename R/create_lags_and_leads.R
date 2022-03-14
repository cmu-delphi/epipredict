#' Create lags and leads of predictors and response
#'
#' @param x Data frame or matrix. Predictor variables.
#' @param y Response vector. Typical usage will "lead" y by the
#'   number of steps forward for the prediction horizon (ahead).
#' @param xy_lags Vector or list. If a vector, the lags will apply
#'   to each column of `x` and to `y`. If a list, it must be of length
#'   `ncol(x)+1` and each component will apply to the requisite predictor.
#'   A `NULL` list element will remove that variable completely from the
#'   result. Negative values will "lead" the variable.
#' @param y_leads Scalar or vector. If a scalar, we "lead" `y` by this
#'   amount. A vector will produce multiple columns of `y` if this is
#'   useful for your model. Negative values will "lag" the variable.
#'
#' @return A `data.frame`.
#' @export
#'
#' @examples
#'
#' x <- 1:20
#' y <- -20:-1
#' time_value <- c(1:18, 20, 21)
#' create_lags_and_leads(x, y, c(1, 2), 1, time_value)
#' create_lags_and_leads(x, y, list(c(1, 2), 1), 1, time_value)
#' create_lags_and_leads(x, y, list(c(-1, 1), NULL), 1, time_value)
#' create_lags_and_leads(x, y, c(1, 2), c(0, 1), time_value)
create_lags_and_leads <- function(x, y, xy_lags, y_leads,
                                  time_value, key_vars = NULL) {

  if (!missing(x)) x <- tibble(x, y) else x <- tibble(y)
  if (!is.list(xy_lags)) xy_lags <- list(xy_lags)
  p = ncol(x)
  assertthat::assert_that(
    length(xy_lags) == 1 || length(xy_lags) == p,
    msg = paste("xy_lags must be either a vector or a list.",
                "If a list, it must have length 1 or `ncol(x) + 1`."))
  xy_lags = rep(xy_lags, length.out = p)

  xdat <- epi_shift(x, xy_lags, time_value, key_vars)
  ydat <- epi_shift(y, -1 * y_leads, time_value, key_vars, "y")

  suppressMessages(dplyr::full_join(ydat, xdat))
}
