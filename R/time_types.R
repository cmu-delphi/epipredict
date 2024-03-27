guess_time_type <- function(time_value) {
  # similar to epiprocess:::guess_time_type() but w/o the gap handling
  arg_is_scalar(time_value)
  if (is.character(time_value)) {
    if (nchar(time_value) <= "10") {
      new_time_value <- tryCatch(
        {
          as.Date(time_value)
        },
        error = function(e) NULL
      )
    } else {
      new_time_value <- tryCatch(
        {
          as.POSIXct(time_value)
        },
        error = function(e) NULL
      )
    }
    if (!is.null(new_time_value)) time_value <- new_time_value
  }
  if (inherits(time_value, "POSIXct")) {
    return("day-time")
  }
  if (inherits(time_value, "Date")) {
    return("day")
  }
  if (inherits(time_value, "yearweek")) {
    return("yearweek")
  }
  if (inherits(time_value, "yearmonth")) {
    return("yearmonth")
  }
  if (inherits(time_value, "yearquarter")) {
    return("yearquarter")
  }
  if (is.numeric(time_value) && all(time_value == as.integer(time_value)) &&
    all(time_value >= 1582)) {
    return("year")
  }
  return("custom")
}

coerce_time_type <- function(x, target_type) {
  if (target_type == "year") {
    if (is.numeric(x)) {
      return(as.integer(x))
    } else {
      return(as.POSIXlt(x)$year + 1900L)
    }
  }
  switch(target_type,
    "day-time" = as.POSIXct(x),
    "day" = as.Date(x),
    "week" = as.Date(x),
    "yearweek" = tsibble::yearweek(x),
    "yearmonth" = tsibble::yearmonth(x),
    "yearquarter" = tsibble::yearquarter(x)
  )
}

validate_date <- function(x, expected, arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  time_type_x <- guess_time_type(x)
  ok <- time_type_x == expected
  if (!ok) {
    cli::cli_abort(c(
      "The {.arg {arg}} was given as a {.val {time_type_x}} while the",
      `!` = "`time_type` of the training data was {.val {expected}}.",
      i = "See {.topic epiprocess::epi_df} for descriptions of these are determined."
    ), call = call)
  }
}
