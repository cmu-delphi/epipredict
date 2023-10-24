validate_forecaster_inputs <- function(epi_data, outcome, predictors) {
  if (!epiprocess::is_epi_df(epi_data)) {
    cli::cli_abort(c(
      "`epi_data` must be an {.cls epi_df}.",
      "!" = "This one is a {.cls {class(epi_data)}}."
    ))
  }
  arg_is_chr(predictors)
  arg_is_chr_scalar(outcome)
  if (!outcome %in% names(epi_data)) {
    cli::cli_abort("{.var {outcome}} was not found in the training data.")
  }
  check <- hardhat::check_column_names(epi_data, predictors)
  if (!check$ok) {
    cli::cli_abort(c(
      "At least one predictor was not found in the training data.",
      "!" = "The following required columns are missing: {.val {check$missing_names}}."
    ))
  }
  invisible(TRUE)
}


arx_lags_validator <- function(predictors, lags) {
  p <- length(predictors)

  if (!is.list(lags)) lags <- list(lags)
  l <- length(lags)
  if (l == 1) {
    lags <- rep(lags, p)
  } else if (length(lags) != p) {
    cli::cli_abort(c(
      "You have requested {p} predictor(s) but {l} different lags.",
      i = "Lags must be a vector or a list with length == number of predictors."
    ))
  } else {
    if (length(lags) == sum(names(lags) != "", na.rm = TRUE)) {
      if (all(predictors %in% names(lags))) {
        lags <- lags[order(match(names(lags), predictors))]
      } else {
        predictors_miss <- setdiff(predictors, names(lags))
        cli::cli_abort(c(
          "If lags is a named list, then all predictors must be present.",
          i = "The predictors are {.var {predictors}}.",
          i = "So lags is missing {.var {predictors_miss}}'."
        ))
      }
    }
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
  bullet <- "\u2022"
  header <- glue::glue("A basic forecaster of type {name}")
  header <- cli::rule(header, line = 2)
  cat_line(header)
  cat("\n")

  date_created <- glue::glue(
    "This forecaster was fit on {format(x$metadata$forecast_created)}"
  )
  cat_line(date_created)
  cat("\n")

  cat_line("Training data was an `epi_df` with")
  cat_line(glue::glue("\u2022 Geography: {x$metadata$training$geo_type},"))
  cat_line(glue::glue("{bullet} Time type: {x$metadata$training$time_type},"))
  cat_line(glue::glue("{bullet} Using data up-to-date as of: {format(x$metadata$training$as_of)}."))

  cat("\n")
  header <- cli::rule("Predictions")
  cat_line(header)
  cat("\n")

  n_geos <- dplyr::n_distinct(x$predictions$geo_value)
  fds <- unique(x$predictions$forecast_date)
  tds <- unique(x$predictions$target_date)

  cat_line(
    glue::glue("A total of {nrow(x$predictions)} predictions are available for")
  )
  cat_line(glue::glue("{bullet} {n_geos} unique geographic regions,"))
  cat_line(glue::glue("{bullet} At forecast dates: {fds},"))
  cat_line(glue::glue("{bullet} For target dates: {tds}."))

  cat("\n")
}
