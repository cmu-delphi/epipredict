#' offset each relevant column by it's appropriate latency
#' works for either adjusting aheads or lags
#' note that this may introduce new NA values when one column is shifted farther than another
#' @param shift_cols a tibble which must have the columns `column`, the name of
#'   the column to adjust, `latency` the latency of the original column relative
#'   to the `forecast_date`, `new_name`, the names in `column` adjusted by the
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
#' @param shift_cols a list of columns to operate on, as created by `construct_shift_tibble`
#' @param new_data the data transformed so far
#' @param forecast_date the forecast date
#' @param latency `NULL`, int, or vector, as described in `step_epi_latency`
#' @param sign_shift -1 if ahead, 1 if lag
#' @return a tibble with columns `column` (relevant shifted names), `shift` (the
#'   amount that one is shifted), `latency` (original columns difference between
#'   the max `time_value` and `forecast_date` (on a per-initial column basis)),
#'   `effective_shift` (shift+latency), and `new_name` (adjusted names with the
#'   effective_shift)
#' @keywords internal
#' @importFrom dplyr rowwise %>%
get_latent_column_tibble <- function(
    shift_cols, new_data, forecast_date, latency,
    sign_shift, info, epi_keys_checked, call = caller_env()) {
  shift_cols <- shift_cols %>% mutate(original_name = glue::glue("{prefix}{shift}_{terms}"))
  if (is.null(latency)) {
    shift_cols <- shift_cols %>%
      rowwise() %>%
      # add the latencies to shift_cols
      mutate(latency = get_latency(
        new_data, forecast_date, original_name, shift, sign_shift, epi_keys_checked
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
      new_name = glue::glue("{prefix}{effective_shift}_{terms}")
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


#' Extract the as_of for the forecast date, and make sure there's nothing very off about it.
#' @keywords internal
set_forecast_date <- function(new_data, info, epi_keys_checked) {
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
  # get the minimum value across the checked epi_keys' maximum time values
  max_time <- new_data %>%
    select(all_of(original_columns)) %>%
    drop_na() %>%
    {
      # null and "" don't work in `group_by`
      if (!is.null(epi_keys_checked) && epi_keys_checked != "") {
        group_by(., get(epi_keys_checked))
      } else {
        .
      }
    } %>%
    summarize(time_value = max(time_value)) %>%
    pull(time_value) %>%
    min()
  forecast_date <- attributes(new_data)$metadata$as_of
  # make sure the as_of is sane
  if (!inherits(forecast_date, class(max_time)) & !inherits(forecast_date, "POSIXt")) {
    cli::cli_abort(paste(
      "the data matrix `forecast_date` value is {forecast_date}, ",
      "and not a valid `time_type` with type ",
      "matching `time_value`'s type of ",
      "{class(max_time)}."
    ))
  }
  if (is.null(forecast_date) || is.na(forecast_date)) {
    cli::cli_warn(paste(
      "epi_data's `forecast_date` was {forecast_date}, setting to ",
      "the latest time value, {max_time}."
    ))
    forecast_date <- max_time
  } else if (forecast_date < max_time) {
    cli::cli_abort(paste(
      "`forecast_date` ({(forecast_date)}) is before the most ",
      "recent data ({max_time}). Remove before ",
      "predicting."
    ))
  }
  # TODO cover the rest of the possible types for as_of and max_time...
  if (class(max_time) == "Date") {
    forecast_date <- as.Date(forecast_date)
  }
  return(forecast_date)
}

#' the latency is also the amount the shift is off by
#' @param sign_shift integer. 1 if lag and -1 if ahead. These represent how you
#'   need to shift the data to bring the 3 day lagged value to today.
#' @keywords internal
get_latency <- function(new_data, forecast_date, column, shift_amount, sign_shift, epi_keys_checked) {
  shift_max_date <- new_data %>%
    drop_na(all_of(column))
  # null and "" don't work in `group_by`
  if (!is.null(epi_keys_checked) && epi_keys_checked != "") {
    shift_max_date <- shift_max_date %>% group_by(get(epi_keys_checked))
  }
  shift_max_date <- shift_max_date %>%
    summarize(time_value = max(time_value)) %>%
    pull(time_value) %>%
    min()
  return(as.integer(sign_shift * (as.Date(forecast_date) - shift_max_date) + shift_amount))
}



#' get the target date while in a layer
get_forecast_date_in_layer <- function(this_recipe, workflow_max_time_value, new_data) {
  max_time_value <- max(
    workflow_max_time_value,
    this_recipe$max_time_value,
    max(new_data$time_value)
  )
  if (this_recipe %>% recipes::detect_step("adjust_latency")) {
    # get the as_of in an `adjust_latency` step, regardless of where
    handpicked_as_of <- map(
      this_recipe$steps,
      function(x) {
        if (inherits(x, "step_adjust_latency")) x$as_of
      }
    ) %>% Filter(Negate(is.null), .)
    if (length(handpicked_as_of) > 0) {
      max_time_value <- handpicked_as_of[[1]]
    } else {
      # if we haven't chosen one, use either the max_time_value or the as_of
      max_time_value <- max(
        max_time_value,
        attributes(new_data)$metadata$as_of
      )
    }
  }
  max_time_value
}
