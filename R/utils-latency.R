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
    select(original_name, latency, new_name) %>%
    pmap(function(original_name, latency, new_name) {
      epi_shift_single(
        x = new_data,
        col = original_name,
        shift_val = latency,
        newname = new_name,
        key_cols = keys
      )
    }) %>%
    map(function(x) zoo::na.trim(x)) %>%
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

#' create a table of the columns to modify, their shifts, and their prefixes
#' @keywords internal
#' @importFrom dplyr tibble
#' @importFrom tidyr unnest
construct_shift_tibble <- function(terms_used, recipe, rel_step_type, shift_name) {
  # for the right step types (either "step_epi_lag" or "step_epi_shift"), grab
  # the useful parameters, including the evaluated column names
  extract_named_rates <- function(recipe_step) {
    if (inherits(recipe_step, rel_step_type)) {
      recipe_columns <- recipes_eval_select(recipe_step$terms, recipe$template, recipe$term_info)
      if (any(recipe_columns %in% terms_used)) {
        return(list(term = recipe_columns, shift = recipe_step[shift_name], prefix = recipe_step$prefix))
      }
    }
    return(NULL)
  }
  rel_list <- recipe$steps %>%
    purrr::map(extract_named_rates) %>%
    unlist(recursive = FALSE) %>%
    split(c("term", "shift", "prefix"))
  relevant_shifts <- tibble(
    terms = lapply(rel_list$term, unname),
    shift = lapply(rel_list$shift, unname),
    prefix = unname(unlist(rel_list$prefix))
  ) %>%
    unnest(c(terms, shift)) %>%
    unnest(shift)
  return(relevant_shifts)
}

#' find the columns added with the lags or aheads, and the amounts they have
#' been changed
#' @param prefix the prefix indicating if we are adjusting lags or aheads
#' @param new_data the data transformed so far
#' @return a tibble with columns `column` (relevant shifted names), `shift` (the
#'   amount that one is shifted), `latency` (original columns difference between
#'   max_time_value and as_of (on a per-initial column basis)),
#'   `effective_shift` (shift+latency), and `new_name` (adjusted names with the
#'   effective_shift)
#' @keywords internal
#' @importFrom dplyr rowwise %>%
#' @importFrom purrr map_lgl
#' @importFrom glue glue
get_latent_column_tibble <- function(
    shift_cols, new_data, as_of, latency,
    sign_shift, info, call = caller_env()) {
  shift_cols <- shift_cols %>% mutate(original_name = glue("{prefix}{shift}_{terms}"))
  if (is.null(latency)) {
    shift_cols <- shift_cols %>%
      rowwise() %>%
      # add the latencies to shift_cols
      mutate(latency = get_latency(
        new_data, as_of, original_name, shift, sign_shift
      )) %>%
      ungroup()
  } else if (length(latency) > 1) {
    # if latency has a length, we assign based on comparing the name in the list with the `terms` column
    shift_cols <- shift_cols %>%
      rowwise() %>%
      mutate(latency = unname(latency[names(latency) == terms])) %>%
      ungroup()
  } else {
    shift_cols <- shift_cols %>% mutate(latency = latency)
  }

  # add the updated names to shift_cols
  shift_cols <- shift_cols %>%
    mutate(
      effective_shift = shift + abs(latency)
    ) %>%
    mutate(
      new_name = glue("{prefix}{effective_shift}_{terms}")
    )
  info <- info %>% select(variable, type, role)
  shift_cols <- left_join(shift_cols, info, by = join_by(original_name == variable))
  if (length(unique(shift_cols$role)) != 1) {
    cli::cli_abort("not all roles are the same!",
      shift_cols = shift_cols
    )
  }
  return(shift_cols)
}


#' extract the as_of, and make sure there's nothing very off about it
#' @keywords internal
set_asof <- function(new_data, info) {
  original_columns <- info %>%
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
    cli::cli_abort("the `time_value` column of `new_data` is empty")
  }
  as_of <- attributes(new_data)$metadata$as_of
  max_time <- max(time_values)
  # make sure the as_of is sane
  if (!inherits(as_of, class(time_values)) & !inherits(as_of, "POSIXt")) {
    cli::cli_abort(glue::glue(
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
