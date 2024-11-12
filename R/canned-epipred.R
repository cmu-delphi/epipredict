validate_forecaster_inputs <- function(epi_data, outcome, predictors) {
  if (!is_epi_df(epi_data)) {
    cli_abort(c(
      "`epi_data` must be an {.cls epi_df}.",
      "!" = "This one is a {.cls {class(epi_data)}}."
    ))
  }
  arg_is_chr_scalar(outcome)
  arg_is_chr(predictors)
  if (!outcome %in% names(epi_data)) {
    cli_abort("{.var {outcome}} was not found in the training data.")
  }
  check <- hardhat::check_column_names(epi_data, predictors)
  if (!check$ok) {
    cli_abort(c(
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
    cli_abort(c(
      "You have requested {p} predictor(s) but {l} different lags.",
      i = "Lags must be a vector or a list with length == number of predictors."
    ))
  } else {
    if (length(lags) == sum(names(lags) != "", na.rm = TRUE)) {
      if (all(predictors %in% names(lags))) {
        lags <- lags[order(match(names(lags), predictors))]
      } else {
        predictors_miss <- setdiff(predictors, names(lags))
        cli_abort(c(
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
#' @importFrom hardhat extract_recipe
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
  fn_meta <- function() {
    cli::cli_ul()
    cli::cli_li("Geography: {.field {x$metadata$training$geo_type}},")
    other_keys <- x$metadata$training$other_keys
    if (!is.null(other_keys) && length(other_keys) > 0L) {
      cli::cli_li("Other keys: {.field {other_keys}},")
    }
    cli::cli_li("Time type: {.field {x$metadata$training$time_type}},")
    cli::cli_li("Using data up-to-date as of: {.field {format(x$metadata$training$as_of)}}.")
    cli::cli_li("With the last data available on {.field {format(max(x$epi_workflow$original_data$time_value))}}")
    cli::cli_end()
  }
  fn_meta()
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
    "For target date{?s}: {.val {tds}},"
  ))
  fit_recipe <- extract_recipe(x$epi_workflow)
  if (detect_step(fit_recipe, "adjust_latency")) {
    is_adj_latency <- map_lgl(fit_recipe$steps, function(x) inherits(x, "step_adjust_latency"))
    latency_step <- fit_recipe$steps[is_adj_latency][[1]]
    # all steps after adjust_latency
    later_steps <- fit_recipe$steps[-(1:which(is_adj_latency))]
    if (latency_step$method == "extend_ahead") {
      step_names <- "step_epi_ahead"
      type_str <- "Aheads"
    } else if (latency_step$method == "extend_lags") {
      step_names <- "step_epi_lag"
      type_str <- "Lags"
    } else {
      step_names <- ""
      type_str <- "columns locf"
    }
    later_steps[[1]]$columns
    valid_columns <- later_steps %>%
      keep(function(x) inherits(x, step_names)) %>%
      purrr::map("columns") %>%
      reduce(c)
    latency_per_base_col <- latency_step$latency_table %>%
      filter(col_name %in% valid_columns) %>%
      mutate(latency = abs(latency))
    if (latency_step$method != "locf" && nrow(latency_per_base_col) > 1) {
      intro_text <- glue::glue("{type_str} adjusted per column: ")
    } else if (latency_step$method != "locf") {
      intro_text <- glue::glue("{type_str} adjusted for ")
    }
    latency_info <- paste0(intro_text, paste(apply(latency_per_base_col, 1, paste0, collapse = "="), collapse = ", "))
    cli::cli_ul(latency_info)
  }
  cli::cli_text("")
}
