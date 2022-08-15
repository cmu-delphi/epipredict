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


