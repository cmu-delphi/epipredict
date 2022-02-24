prob_ar <- function(y, lags = c(0, 7, 14), ahead = 7, min_train_window = 20,
                    lower_level = 0.05, upper_level = 0.95, symmetrize = TRUE,
                    nonneg = TRUE) {
  # Return NA if insufficient training data
  if (length(y) < min_train_window + max(lags) + ahead) {
    return(data.frame(point = NA, lower = NA, upper = NA))
  }

  # Build features and response for the AR model
  dat <- do.call(
    data.frame,
    purrr::map(lags, function(lag) dplyr::lag(y, n = lag))
  )
  dat$y <- dplyr::lead(y, n = ahead)

  # Now fit the AR model and make a prediction
  obj <- lm(y ~ ., data = dat)
  point <- predict(obj, newdata = tail(dat, 1))

  # Compute a band
  r <- residuals(obj)
  s <- ifelse(symmetrize, -1, NA) # Should the residuals be symmetrized?
  q <- quantile(c(r, s * r), probs = c(lower_level, upper_level), na.rm = TRUE)
  lower <- point + q[1]
  upper <- point + q[2]

  # Clip at zero if we need to, then return
  if (nonneg) {
    point = max(point, 0)
    lower = max(lower, 0)
    upper = max(upper, 0)
  }
  return(data.frame(point = point, lower = lower, upper = upper))
}
