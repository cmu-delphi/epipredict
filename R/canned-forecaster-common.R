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
