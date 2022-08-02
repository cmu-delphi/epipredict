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
#' @return A data frame of point (and optionally interval) forecasts at a single
#'   ahead (unique horizon) for each unique combination of `key_vars`.
#' @export
arx_forecaster <- function(x, y, key_vars, time_value,
                           args = arx_args_list()) {

  # TODO: function to verify standard forecaster signature inputs

  assign_arg_list(args)
  if (is.null(key_vars)) { # this is annoying/repetitive, seemingly necessary?
    keys <- NULL
    distinct_keys <- tibble(.dump = NA)
  } else {
    keys <- tibble::tibble(key_vars)
    distinct_keys <- dplyr::distinct(keys)
  }

  # Return NA if insufficient training data
  if (length(y) < min_train_window + max_lags + ahead) {
    qnames <- probs_to_string(levels)
    out <- dplyr::bind_cols(distinct_keys, point = NA) %>%
      dplyr::select(!dplyr::any_of(".dump"))
    return(enframer(out, qnames))
  }

  dat <- create_lags_and_leads(x, y, lags, ahead, time_value, keys)
  dat$x0 <- 1

  obj <- stats::lm(
    y1 ~ . + 0,
    data = dat %>% dplyr::select(starts_with(c("x", "y")))
  )

  point <- make_predictions(obj, dat, time_value, keys)

  # Residuals, simplest case, requires
  # 1. same quantiles for all keys
  # 2. `residuals(obj)` works
  r <- residuals(obj)
  q <- residual_quantiles(r, point, levels, symmetrize)

  # Harder case requires handling failures of 1 and or 2, neither implemented
  # 1. different quantiles by key, need to bind the keys, then group_modify
  # 2 fails. need to bind the keys, grab, y and yhat, subtract
  if (nonneg) {
    q <- dplyr::mutate(q, dplyr::across(dplyr::everything(), ~ pmax(.x, 0)))
  }

  return(
    dplyr::bind_cols(distinct_keys, q) %>%
      dplyr::select(!dplyr::any_of(".dump"))
  )
}


#' ARX forecaster argument constructor
#'
#' Constructs a list of arguments for [arx_forecaster()].
#'
#' @template param-lags
#' @template param-ahead
#' @template param-min_train_window
#' @param forecast_date The date on which the forecast is created. The default
#'   `NULL` will attempt to determine this automatically.
#' @param target_date The date for which the forecast is intended. The default
#'   `NULL` will attempt to determine this automatically.
#' @template param-levels
#' @template param-symmetrize
#' @template param-nonneg
#' @param quantile_by_key Should separate quantiles be produced for each
#'   `geo_value` (and additional `keys`) combination
#'
#' @return A list containing updated parameter choices.
#' @export
#'
#' @examples
#' arx_args_list()
#' arx_args_list(symmetrize = FALSE)
#' arx_args_list(levels = c(.1, .3, .7, .9), min_train_window = 120)
arx_args_list <- function(lags = c(0L, 7L, 14L),
                          ahead = 7L,
                          min_train_window = 20L,
                          forecast_date = NULL,
                          target_date = NULL,
                          levels = c(0.05, 0.95),
                          symmetrize = TRUE,
                          nonneg = TRUE,
                          quantile_by_key = TRUE) {

  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  arg_is_scalar(ahead, min_train_window, symmetrize, nonneg, quantile_by_key)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, min_train_window, lags)
  arg_is_lgl(symmetrize, nonneg, quantile_by_key)
  arg_is_probabilities(levels, allow_null = TRUE)

  max_lags <- max(lags)

  list(
    lags = .lags, ahead = as.integer(ahead),
    min_train_window = min_train_window,
    levels = levels,
    forecast_date = forecast_date, target_date = target_date,
    symmetrize = symmetrize, nonneg = nonneg,
    max_lags = max_lags,
    quantile_by_key = quantile_by_key
  )
}
