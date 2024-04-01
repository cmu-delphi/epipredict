#' offset each relevant column by it's appropriate latency
#' works for either adjusting aheads or lags
#' @param shift_cols a tibble which must have the columns `column`, the name of
#'   the column to adjust, `latency` the latency of the original column relative
#'   to the `as_of` date, `new_name`, the names in `column` adjusted by the
#'   latencies `latency`
#' @param new_data just what is says
#' @param keys the variables which are used as keys
#' @keywords internal
extend_either <- function(new_data, shift_cols, keys) {
  shifted <-
    shift_cols %>%
    select(-shifts, -effective_shift) %>%
    pmap(\(original_name, latency, new_name) {
      epi_shift_single(
        x = new_data,
        col = original_name,
        shift_val = latency,
        newname = new_name,
        key_cols = keys
      )
    }) %>%
    map(\(x) zoo::na.trim(x)) %>% # TODO need to talk about this
    reduce(
      dplyr::full_join,
      by = keys
    )

  return(new_data %>%
    select(-all_of(shift_cols$original_name)) %>% # drop the original versions
    dplyr::full_join(shifted, by = keys) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::ungroup())
}

#' find the columns added with the lags or aheads, and the amounts they have
#' been changed
#' @param prefix the prefix indicating if we are adjusting lags or aheads
#' @param new_data the data transformed so far
#' @return a tibble with columns `column` (relevant shifted names), `shift` (the
#'   amount that one is shifted), `latency` (original columns difference between
#'   max_time_value and as_of (on a per-initial column basis)),
#'   `effective_shift` (shifts+latency), and `new_name` (adjusted names with the
#'   effective_shift)
#' @keywords internal
#' @importFrom stringr str_match
#' @importFrom dplyr rowwise %>%
get_shifted_column_tibble <- function(
    prefix, new_data, terms_used, as_of, sign_shift, call = caller_env()) {
  relevant_columns <- names(new_data)[grepl(prefix, names(new_data))]
  to_keep <- rep(FALSE, length(relevant_columns))
  for (col_name in terms_used) {
    to_keep <- to_keep | grepl(col_name, relevant_columns)
  }
  relevant_columns <- relevant_columns[to_keep]
  if (length(relevant_columns) == 0) {
    cli::cli_abort("There is no column(s) {terms_used}.",
      current_column_names = names(new_data),
      class = "epipredict_adjust_latency_nonexistent_column_used",
      call = call
    )
  }
  # TODO ask about a less jank way to do this
  shift_amounts <- as.integer(stringr::str_match(
    relevant_columns,
    "_\\d+_"
  ) %>%
    `[`(, 1) %>%
    stringr::str_match("\\d+") %>%
    `[`(, 1))
  shift_cols <- dplyr::tibble(
    original_name = relevant_columns,
    shifts = shift_amounts
  )
  shift_cols %<>%
    rowwise() %>%
    # add the latencies to shift_cols
    mutate(latency = get_latency(
      new_data, as_of, original_name, shifts, sign_shift
    )) %>%
    ungroup() %>%
    # add the updated names to shift_cols
    mutate(
      effective_shift = shifts + abs(latency)
    ) %>%
    mutate(
      new_name = adjust_name(prefix, original_name, effective_shift)
    )
  return(shift_cols)
}


#' extract the as_of, and make sure there's nothing very off about it
get_asof <- function(object, new_data) {
  original_columns <- object$info %>%
    filter(source == "original") %>%
    pull(variable)
  # make sure that there's enough column names
  if (length(original_columns) < 3) {
    cli::cli_abort(glue::glue(
      "The original columns of `time_value`, ",
      "`geo_value` and at least one signal. The current colums are \n",
      paste(capture.output(object$info), collapse = "\n\n")
    ))
  }
  # the source data determines the actual time_values
  # these are the non-na time_values;
  time_values <- new_data %>%
    select(all_of(original_columns)) %>%
    drop_na() %>%
    pull(time_value)
  if (length(time_values) <= 0) {
    rlang::abort("the `time_value` column of `new_data` is empty")
  }
  as_of <- attributes(new_data)$metadata$as_of
  max_time <- max(time_values)
  # make sure the as_of is sane
  # TODO decide on these checks
  if (!inherits(as_of, class(time_values))) {
    rlang::abort(glue::glue(
      "the data matrix `as_of` value is {as_of}, ",
      "and not a valid `time_type` with type ",
      "matching `time_value`'s type of ",
      "{typeof(new_data$time_value)}."
    ))
  }
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
  # TODO cover the rest of the possible types for as_of and max_time...
  if (class(time_values) == "Date") {
    as_of <- as.Date(as_of)
  }
  return(as_of)
}

#' adjust the shifts by latency for the names in column assumes e.g.
#' `"lag_6_case_rate"` and returns something like `"lag_10_case_rate"`
#' @keywords internal
adjust_name <- function(prefix, column, effective_shift) {
  pattern <- paste0(prefix, "\\d+", "_")
  adjusted_shifts <- paste0(prefix, effective_shift, "_")
  stringi::stri_replace_all_regex(
    column,
    pattern, adjusted_shifts
  )
}

#' the latency is also the amount the shift is off by
#' @param sign_shift integer. 1 if lag and -1 if ahead. These represent how you
#'   need to shift the data to bring the 3 day lagged value to today.
#' @keywords internal
get_latency <- function(new_data, as_of, column, shift_amount, sign_shift) {
  shift_max_date <- new_data %>%
    drop_na(all_of(column)) %>%
    pull(time_value) %>%
    max()
  return(as.integer(sign_shift * (as_of - shift_max_date) + shift_amount))
}
