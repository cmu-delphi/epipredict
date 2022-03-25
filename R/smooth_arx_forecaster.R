#' Smooth AR forecaster with optional covariates
#'
#' @param x Covariates. Allowed to be missing (resulting in AR on `y`).
#' @param y Response.
#' @param key_vars Factor(s). A prediction will be made for each unique
#'   combination.
#' @param time_value the time value associated with each row of measurements.
#' @param args Additional arguments specifying the forecasting task. Created
#'   by calling `smooth_arx_args_list()`.
#'
#' @return
#' @export
smooth_arx_forecaster <- function(x, y, key_vars, time_value,
                                  args = smooth_arx_args_list()) {

  # TODO: function to verify standard forecaster signature inputs

  # get all args into the environment
  for (a in seq_along(args)) assign(names(args)[a], args[[a]])

  # Return NA if insufficient training data (likely specific to the args)
  if (length(y) < min_train_window + max_lags + max(ahead)) {
    qnames <- probs_to_string(levels)
    if (is.null(key_vars)) out <- tibble(point = NA)
    else out = dplyr::bind_cols(dplyr::distinct(tibble(key_vars)), point = NA)
    return(enframer(out, qnames))
  }

  dat <- create_lags_and_leads(x, y, lags, ahead, time_value, key_vars)
  if (intercept) dat$x0 <- 1

  # Create H
  H <- splines::bs(ahead, df = df, intercept = TRUE)
  if (kronecker_version) {
    stop("not yet implemented")
  } else {
    dat <- df_mat_mul(dat, H, "y", starts_with("y"))
    ny <- grab_names(dat, starts_with("y"))
    nx <- grab_names(dat, starts_with("x"))
    form <- as.formula(paste(
      "cbind(", paste(ny, collapse = ","), ") ~ ", # multivariate y
      paste(nx, collapse = "+"), "+ 0"))
    obj <- stats::lm(form, data = dat %>%
                       dplyr::select(starts_with(c("x","y"))))
  }


  point <- tcrossprod(make_predictions(obj, dat, time_value, key_vars), H) %>%
    as.data.frame()

  # Residuals, simplest case,
  # 1. same quantiles for all keys
  # 2. `residuals(obj)` works
  # (3.) didn't ask for quantiles
  r <- tcrossprod(residuals(obj), H) %>%
    as.data.frame() %>%
    magrittr::set_names(ahead)
  q <- purrr::map2_dfr(
    r, point, residual_quantiles(.x, .y, levels, symmetrize), .id = "ahead"
  )

  # Harder case,
  # 1. different quantiles by key, need to bind the keys, then group_modify
  # 2 fails. need to bind the keys, grab, y and yhat, subtract
  if (nonneg) q <- dplyr::mutate(q, dplyr::across(!ahead, ~ pmax(.x, 0)))

  if (is.null(key_vars)) return(q)
  else {
    return(dplyr::bind_cols(dplyr::distinct(data.frame(key_vars)), q))
  }
}


smooth_arx_args_list <- function(
  lags = c(0, 7, 14), ahead = 1:28,
  df = 4, kronecker_version = FALSE,
  min_train_window = 20,
  levels = c(0.05, 0.95), intercept = TRUE,
  symmetrize = TRUE,
  nonneg = TRUE,
  quantile_by_key = FALSE) {

  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  arg_is_scalar(df, min_train_window)
  arg_is_nonneg_int(df, ahead, min_train_window, lags)
  arg_is_lgl(intercept, symmetrize, nonneg, kronecker_version)
  arg_is_probabilities(levels, allow_null=TRUE)

  max_lags <- max(lags)

  if (length(ahead) == 1)
    stop("Smoothing is immaterial for only a single ahead. You\n",
         "may want `arx_forecaster()` instead.")
  if (df >= length(ahead))
    stop("Smoothing requires fewer degrees of freedom then ahead values.")

  list(lags = .lags, ahead = ahead, df = df,
       min_train_window = min_train_window,
       kronecker_version = kronecker_version,
       levels = levels, intercept = intercept,
       symmetrize = symmetrize, nonneg = nonneg,
       max_lags = max_lags)
}
