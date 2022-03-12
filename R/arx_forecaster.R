#' AR forecaster with optional covariates
#'
#' @param x Covariates. Allowed to be missing (resulting in AR on `y`).
#' @param y Response.
#' @param key_vars Factor(s). A prediction will be made for each unique
#'   combination.
#' @param time_value the time value associated with each row of measurements.
#' @param args Additional arguments specifying the forecasting task. Created
#'   by calling `arx_args_list()`.
#'
#' @return
#' @export
arx_forecaster <- function(x, y, key_vars, time_value,
                           args = arx_args_list()) {

  # TODO: function to verify standard forecaster signature inputs

  # get all args into the environment
  for (a in seq_along(args)) assign(names(args)[a], args[[a]])

  # Return NA if insufficient training data (likely specific to the args)
  if (length(y) < min_train_window + max_lags + ahead) {
    qnames <- probs_to_string(levels)
    # note: key_vars must be a df?!?
    out = dplyr::bind_cols(dplyr::distinct(data.frame(key_vars)), point = NA)
    return(enframer(out, qnames))
  }

  dat <- create_lags_and_leads(x, y, lags, ahead)
  if (intercept) dat$x0 <- 1
  obj <- stats::lm(y ~ . + 0, data = dat)
  point <- make_prediction_with_S3(obj, dat, key_vars, time_value)

  # TODO: do we want to allow separate by geo?
  q <- residual_quantiles_with_S3(obj, dat, point, levels, symmetrize)

  if (nonneg)
    q <- dplyr::mutate(q, dplyr::across(dplyr::everything(), ~ pmax(.x, 0)))

  return(dplyr::bind_cols(dplyr::distinct(data.frame(key_vars)), q))
}


arx_args_list <- function(
  lags = c(0, 7, 14), ahead = 7, min_train_window = 20,
  levels = c(0.05, 0.95), intercept = TRUE,
  symmetrize = TRUE,
  nonneg = TRUE) {

  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  arg_is_scalar(ahead, min_train_window)
  arg_is_nonneg_int(ahead, min_train_window, lags)
  arg_is_lgl(intercept, symmetrize, nonneg)
  arg_is_probabilities(levels, allow_null=TRUE)

  max_lags <- max(lags)

  list(lags = .lags, ahead = ahead, min_train_window = min_train_window,
       levels = levels, intercept = intercept,
       symmetrize = symmetrize, nonneg = nonneg,
       max_lags = max_lags)
}
