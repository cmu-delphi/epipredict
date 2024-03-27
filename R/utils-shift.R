#' various ways of handling differences between the `as_of` date and the maximum
#' time value
#' @description
#' adjust the ahead so that we will be predicting `ahead` days after the `as_of`
#'   date, rather than relative to the last day of data
#' @keywords internal
adjust_latency <- function(object, new_data) {
  method <- object$latency_adjustment
  ahead <- object$ahead
  if (is.na(method) || is.null(method) || method == "None") {
    return(object$ahead)
  } else if (method == "extend_ahead") {
    as_of <- attributes(new_data)$metadata$as_of
    if (FALSE && (typeof(as_of) != typeof(new_data$time_value))) {
      rlang::abort(glue::glue(
        "the data matrix `as_of` value is {as_of}, ",
        "and not a valid `time_type` with type ",
        "matching `time_value`'s type of ",
        "{typeof(new_data$time_value)}."
      ))
    }
    # adjust the ahead so that we're predicting relative to the as_of date,
    # rather
    # than the last day of data
    time_values <- new_data$time_value
    if (length(time_values) > 0) {
      max_time <- max(time_values)
      shift_amount <- as.Date(as_of) - max_time
      if (is.null(as_of) || is.na(as_of)) {
        cli::cli_warn(glue::glue(
          "epi_data's `as_of` was {as_of}, setting to ",
          "the latest time value, {max_time}."
        ))
        as_of <- max_time
      } else if (as_of < max_time) {
        cli::cli_abort(glue::glue(
          "`as_of` ({(as_of)}) is before the most ",
          "recent data ({max_time}). Remove before ",
          "predicting."
        ))
      }
      effective_ahead <- as.integer(shift_amount + ahead)
      time_type <- attributes(new_data)$metadata$time_type

      if ((grepl("day", time_type) && (shift_amount >= 10)) ||
        (grepl("week", time_type) && (shift_amount >= 4)) ||
        ((time_type == "yearmonth") && (shift_amount >= 2)) ||
        ((time_type == "yearquarter") && (shift_amount >= 1)) ||
        ((time_type == "year") && (shift_amount >= 1))) {
        cli::cli_warn(c(
          "!" = glue::glue(
            "The ahead has been adjusted by {shift_amount}, ",
            "which is questionable for it's `time_type` of ",
            "{time_type}"
          ),
          "i" = "input ahead: {ahead}",
          "i" = "shifted ahead: {effective_ahead}",
          "i" = "max_time = {max_time} -> as_of = {as_of}"
        ))
      }
      return(effective_ahead)
    } else {
      rlang::abort("the `time_value` column of `new_data` is empty")
    }
  } else {
    rlang::abort(glue::glue(
      "Latency adjustment method {method} has not yet ",
      "been implemented for `step_epi_ahead`."
    ))
  }
}
