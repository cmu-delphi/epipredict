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
  nm <- names(x)
  for (i in seq_along(x)) {
    if (is.null(x[[i]])) x[[i]] <- "NULL"
    if (length(x[[i]]) == 0L) x[[i]] <- "_empty_"
    cli::cli_bullets(c("*" = "{nm[[i]]} : {.val {x[[i]]}}"))
  }
}

#' @export
print.canned_epipred <- function(x, name, ...) {
  d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
  cli::cli_rule("A basic forecaster of type {name}")
  cli::cli_end(d)
  cli::cli_text("")
  cli::cli_text(
    "This forecaster was fit on {.field {format(x$metadata$forecast_created)}}."
  )
  cli::cli_text("")
  cli::cli_text("Training data was an {.cls epi_df} with:")
  cli::cli_ul(c(
    "Geography: {.field {x$metadata$training$geo_type}},",
    "Time type: {.field {x$metadata$training$time_type}},",
    "Using data up-to-date as of: {.field {format(x$metadata$training$as_of)}}."
  ))
  cli::cli_text("")

  cli::cli_rule("Predictions")
  cli::cli_text("")

  n_geos <- dplyr::n_distinct(x$predictions$geo_value)
  fds <- cli::cli_vec(
    unique(x$predictions$forecast_date),
    list("vec-trunc" = 5)
  )
  tds <- cli::cli_vec(
    unique(x$predictions$target_date),
    list("vec-trunc" = 5)
  )

  cli::cli_text(c(
    "A total of {.val {nrow(x$predictions)}} prediction{?s}",
    " {?is/are} available for"
  ))
  cli::cli_ul(c(
    "{.val {n_geos}} unique geographic region{?s},",
    "At forecast date{?s}: {.val {fds}},",
    "For target date{?s}: {.val {tds}}."
  ))
  cli::cli_text("")
}
