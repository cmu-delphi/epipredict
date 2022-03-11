prob_arx <- function(x, y, geo_value, time_value, lags = c(0, 7, 14),
                     ahead = 7, min_train_window = 20, lower_level = 0.05,
                     upper_level = 0.95, symmetrize = TRUE, nonneg = TRUE) {

  assertthat::assert_that(rlang::is_scalar_atomic(ahead),
                          msg = "ahead must be a scalar")
  if (!(ahead == round(ahead))) {
    warning("ahead is not an integer, rounding...")
    ahead <- round(ahead)
  }
  # Return NA if insufficient training data
  if (length(y) < min_train_window + max(lags) + ahead) {
    return(data.frame(geo_value = unique(geo_value),
      point = NA, lower = NA, upper = NA))
  }

  dat <- create_lags_and_leads(x, y, lags, ahead)
  obj <- lm(y ~ ., data = dat)

  # Use LOCF to fill NAs in the latest feature values, make a prediction
  data.table::setnafill(dat, type = "locf")
  dat <- cbind(dat, data.frame(geo_value, time_value))
  point <- predict(obj, newdata = dat %>%
                     dplyr::group_by(geo_value) %>%
                     dplyr::filter(time_value == max(time_value)))

  # Compute a band
  # TODO: separate by geo?
  r <- residuals(obj)
  s <- ifelse(symmetrize, -1, NA) # Should the residuals be symmetrized?
  q <- quantile(c(r, s * r), probs = c(lower_level, upper_level), na.rm = TRUE)
  lower <- point + q[1]
  upper <- point + q[2]

  # Clip at zero if we need to, then return
  if (nonneg) {
    point = pmax(point, 0)
    lower = pmax(lower, 0)
    upper = pmax(upper, 0)
  }
  return(data.frame(geo_value = unique(geo_value), # Must include geo value!
                    point = point, lower = lower, upper = upper))
}
