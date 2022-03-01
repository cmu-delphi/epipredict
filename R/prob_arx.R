prob_arx <- function(x, y, geo_value, time_value, lags = c(0, 7, 14),
                     ahead = 7, min_train_window = 20, lower_level = 0.05,
                     upper_level = 0.95, symmetrize = TRUE, nonneg = TRUE) {

  # Return NA if insufficient training data
  if (length(y) < min_train_window + max(lags) + ahead) {
    return(data.frame(geo_value = unique(geo_value),
      point = NA, lower = NA, upper = NA))
  }

  # Useful transformations
  if (!missing(x)) x <- data.frame(x, y)
  else x <- data.frame(y)
  if (!is.list(lags)) lags <- list(lags)
  lags = rep(lags, length.out = ncol(x))

  # Build features and response for the AR model, and then fit it
  dat <- do.call(
    data.frame,
    unlist( # Below we loop through and build the lagged features
      purrr::map(1:ncol(x), function(i) {
        purrr::map(lags[[i]], function(lag) dplyr::lag(x[,i], n = lag))
      }),
      recursive = FALSE
    )
  )
  dat$y <- dplyr::lead(y, n = ahead)
  obj <- lm(y ~ ., data = dat)

  # Use LOCF to fill NAs in the latest feature values, make a prediction
  data.table::setnafill(dat, type = "locf")
  dat <- cbind(dat, data.frame(geo_value, time_value))
  point <- predict(obj, newdata = dat %>%
                     dplyr::group_by(geo_value) %>%
                     dplyr::filter(time_value == max(time_value)))

  # Compute a band
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
