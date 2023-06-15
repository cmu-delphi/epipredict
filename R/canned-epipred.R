validate_forecaster_inputs <- function(epi_data, outcome, predictors) {
  if (!epiprocess::is_epi_df(epi_data)) {
    cli::cli_abort(c(
      "`epi_data` must be an {.cls epi_df}.",
      "!" = "This one is a {.cls {class(epi_data)}}."
    ))
  }
  arg_is_chr(predictors)
  arg_is_chr_scalar(outcome)
  if (!outcome %in% names(epi_data))
    cli::cli_abort("{outcome} was not found in the training data.")
  check <- hardhat::check_column_names(epi_data, predictors)
  if (!check$ok) {
    cli::cli_abort(c(
      "At least one predictor was not found in the training data.",
      "!" = "The following required columns are missing: {check$missing_names}."
    ))
  }
  invisible(TRUE)
}


arx_lags_validator <- function(predictors, lags) {
  p <- length(predictors)
  if (!is.list(lags)) lags <- list(lags)
  l <- length(lags)
  if (l == 1) lags <- rep(lags, p)
  else if (length(lags) != p) {
    cli::cli_abort(c(
      "You have requested {p} predictor(s) but {l} different lags.",
      i = "Lags must be a vector or a list with length == number of predictors."
    ))
  }
  lags
}


#' @export
print.alist <- function(x, ...) {
  utils::str(x)
}

#' @export
print.canned_epipred <- function(x, name, ...) {
  cat("\n")
  cli::cli_rule("A basic forecaster of type {.pkg {name}}")

  cat("\n")
  cli::cli_text(
    "This forecaster was fit on {.val {format(x$metadata$forecast_created)}}."
  )
  cat("\n")
  cli::cli_text("Training data was an {.cls epi_df} with ")
  cli::cli_ul(c(
    "Geography: {.val {x$metadata$training$geo_type}},",
    "Time type: {.val {x$metadata$training$time_type}},",
    "Using data up-to-date as of: {.val {format(x$metadata$training$as_of)}}."
  ))

  cat("\n")
  cli::cli_rule("Predictions")
  n_geos <- dplyr::n_distinct(x$predictions$geo_value)
  fds <- unique(x$predictions$forecast_date)
  tds <- unique(x$predictions$target_date)

  cat("\n")
  cli::cli_text("A total of {nrow(x$predictions)} predictions are available for")
  cli::cli_ul(c(
    "{n_geos} unique geographic regions,",
    "At forecast dates: {.val {fds}},",
    "For target dates: {.val {tds}}."
  ))
  cat("\n")
}
