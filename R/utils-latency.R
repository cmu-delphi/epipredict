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


#' Extract the as_of for the forecast date, and make sure there's nothing very off about it.
#' @keywords internal
#' @importFrom dplyr select
set_forecast_date <- function(new_data, info, epi_keys_checked, latency) {
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
    drop_na()
  # null and "" don't work in `group_by`
  if (!is.null(epi_keys_checked) && (epi_keys_checked != "")) {
    max_time <- max_time %>% group_by(get(epi_keys_checked))
  }
  max_time <- max_time %>%
    summarise(time_value = max(time_value)) %>%
    pull(time_value) %>%
    min()
  if (is.null(latency)) {
    forecast_date <- attributes(new_data)$metadata$as_of
  } else {
    forecast_date <- max_time + latency
  }
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
  if (inherits(max_time, "Date")) {
    forecast_date <- as.Date(forecast_date)
  }
  return(forecast_date)
}

#' the latency is also the amount the shift is off by
#' @param sign_shift integer. 1 if lag and -1 if ahead. These represent how you
#'   need to shift the data to bring the 3 day lagged value to today.
#' @keywords internal
get_latency <- function(new_data, forecast_date, column, sign_shift, epi_keys_checked) {
  shift_max_date <- new_data %>%
    drop_na(all_of(column))
  # null and "" don't work in `group_by`
  if (!is.null(epi_keys_checked) && epi_keys_checked != "") {
    shift_max_date <- shift_max_date %>% group_by(get(epi_keys_checked))
  }
  shift_max_date <- shift_max_date %>%
    summarise(time_value = max(time_value)) %>%
    pull(time_value) %>%
    min()
  return(as.integer(sign_shift * (as.Date(forecast_date) - shift_max_date)))
}



#' get the target date while in a layer
#' @param this_recipe the recipe to check for `step_adjust_latency`
#' @param workflow_max_time_value the `max_time` value coming out of the fit
#'   workflow (this will be the maximal time value in a potentially different
#'   dataset)
#' @param new_data the data we're currently working with, from which we'll take
#'   a potentially different max_time_value
#' @keywords internal
get_forecast_date_in_layer <- function(this_recipe, workflow_max_time_value, new_data) {
  max_time_value <- max(
    workflow_max_time_value,
    this_recipe$max_time_value,
    max(new_data$time_value)
  )
  if (this_recipe %>% recipes::detect_step("adjust_latency")) {
    # get the as_of in an `adjust_latency` step, regardless of where
    handpicked_forecast_date <- map(
      this_recipe$steps,
      function(x) {
        if (inherits(x, "step_adjust_latency")) x$forecast_date
      }
    ) %>% Filter(Negate(is.null), .)
    if (length(handpicked_forecast_date) > 0) {
      max_time_value <- handpicked_forecast_date[[1]]
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


fill_locf <- function(x, forecast_date) {
  cannot_be_used <- x %>%
    dplyr::filter(forecast_date - time_value <= n_recent) %>%
    dplyr::mutate(fillers = forecast_date - time_value > keep) %>%
    dplyr::summarise(
      dplyr::across(
        -tidyselect::any_of(epi_keys(recipe)),
        ~ all(is.na(.x[fillers])) & is.na(head(.x[!fillers], 1))
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(-fillers) %>%
    dplyr::summarise(dplyr::across(
      -tidyselect::any_of(epi_keys(recipe)), ~ any(.x)
    )) %>%
    unlist()
  x <- tidyr::fill(x, !time_value)
}

pad_to_end <- function(x, groups, end_date) {
  itval <- epiprocess:::guess_period(c(x$time_value, end_date), "time_value")
  completed_time_values <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(groups))) %>%
    dplyr::summarise(
      time_value = rlang::list2(
        time_value = seq_null_swap(max(time_value) + itval, end_date, itval)
      )
    ) %>%
    unnest("time_value") %>%
    mutate(time_value = vctrs::vec_cast(time_value, x$time_value))

  dplyr::bind_rows(x, completed_time_values) %>%
    dplyr::arrange(dplyr::across(tidyselect::all_of(c("time_value", groups))))
}

seq_null_swap <- function(from, to, by) {
  if (from > to) {
    return(NULL)
  }
  seq(from = from, to = to, by = by)
}
