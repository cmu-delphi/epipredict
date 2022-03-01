create_lags_and_leads <- function(x, y, lags, ahead) {
  # NOTE: if lags last element is NULL, we don't lag y
  # Useful transformations
  if (!missing(x)) x <- data.frame(x, y) else x <- data.frame(y)
  if (!is.list(lags)) lags <- list(lags)
  lags = rep(lags, length.out = ncol(x))
  px = ncol(x)

  # Build features and response for the AR model, and then fit it
  dat <- do.call(
    data.frame,
    unlist( # Below we loop through and build the lagged features
      purrr::map(1:px, function(i) {
        purrr::map(lags[[i]], function(lag) dplyr::lag(x[,i], n = lag))
      }),
      recursive = FALSE)) %>%
    magrittr::set_names(paste0("x", 1:length(unlist(lags))))

  y <- suppressMessages(purrr::map_dfc(ahead, ~ dplyr::lead(y, n = .x)))
  if (ncol(y) > 1) names(y) <- paste0("y", 1:ncol(y)) else names(y) <- "y"

  return(dplyr::bind_cols(y, dat))
}
