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
    map(extract_named_rates) %>%
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

#
adjust_recipe_latency_before_bake <- function(rec) {
  if (detect_step(rec, "adjust_latency")) {
    step_types <- tidy(rec)$type
    # can only be 1, or we would have aborted on recipe creation
    latency_step <- rec$steps[[which(step_types == "adjust_latency")]]
    latency_table <- latency_step$latency_table
    method <- latency_step$method
    if (method == "extend_ahead") {
      loc <- which(step_types == "epi_ahead")
      sign_shift <- -1
    } else if (method == "extend_lags") {
      loc <- which(step_types == "epi_lag")
      sign_shift <- 1
    }
    if (method != "locf") {
      for (s in loc) {
        pfx <- rec$steps[[s]]$prefix
        sgrid <- rec$steps[[s]]$shift_grid
        sgrid <- sgrid %>%
          left_join(latency_table, by = join_by(col == col_name)) %>%
          tidyr::replace_na(list(latency = 0)) %>%
          mutate(shift_val = shift_val + latency, amount = NULL, latency = NULL)
        rec$steps[[s]]$shift_grid <- sgrid
      }
    }
  }
  rec
}

#' Extract the as_of for the forecast date, and make sure there's nothing very off about it.
#' @keywords internal
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @importFrom utils capture.output
get_forecast_date <- function(new_data, info, epi_keys_checked, latency, columns = NULL) {
  if (is.null(columns)) {
    columns <- info %>%
      filter(source == "original") %>%
      pull(variable)
    # make sure that there's enough column names
    if (length(columns) < 3) {
      cli_abort(
        glue::glue(
          "The original columns of `time_value`, ",
          "`geo_value` and at least one signal. The current colums are \n",
          paste(capture.output(object$info), collapse = "\n\n")
        ),
        class = "epipredict__get_forecast_date__too_few_data_columns"
      )
    }
  }
  # the source data determines the actual time_values
  # these are the non-na time_values;
  # get the minimum value across the checked epi_keys' maximum time values
  max_time <- new_data %>%
    select(all_of(columns)) %>%
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
    cli_abort(
      paste(
        "the data matrix `forecast_date` value is {forecast_date}, ",
        "and not a valid `time_type` with type ",
        "matching `time_value`'s type of ",
        "{class(max_time)}."
      ),
      class = "epipredict__get_forecast_date__wrong_time_value_type_error"
    )
  }
  if (is.null(forecast_date) || is.na(forecast_date)) {
    cli_warn(
      paste(
        "epi_data's `forecast_date` was {forecast_date}, setting to ",
        "the latest time value, {max_time}."
      ),
      class = "epipredict__get_forecast_date__max_time_warning"
    )
    forecast_date <- max_time
  } else if (forecast_date < max_time) {
    cli_abort(
      paste(
        "`forecast_date` ({(forecast_date)}) is before the most ",
        "recent data ({max_time}). Remove before ",
        "predicting."
      ),
      class = "epipredict__get_forecast_date__misordered_forecast_date_error"
    )
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
  forecast_date <- as.Date(max(
    workflow_max_time_value,
    this_recipe$max_time_value,
    max(new_data$time_value)
  ))
  if (this_recipe %>% recipes::detect_step("adjust_latency")) {
    # get the as_of in an `adjust_latency` step, regardless of where
    handpicked_forecast_date <- map(
      this_recipe$steps,
      function(x) {
        if (inherits(x, "step_adjust_latency")) x$forecast_date
      }
    ) %>% Filter(Negate(is.null), .)
    if (length(handpicked_forecast_date) > 0) {
      forecast_date <- handpicked_forecast_date[[1]]
    } else {
      # if we haven't chosen one, use either the max_time_value or the as_of
      forecast_date <- max(
        forecast_date,
        attributes(new_data)$metadata$as_of
      )
    }
  }
  forecast_date
}


#' pad every group at the right interval
#' @description
#' Perform last observation carried forward on a group by group basis. It uses
#'   `guess_period` to find the appropriate interval to fill-forward by. It
#'   maintains the grouping structure it recieves. It does *not* fill any
#'   "interior" `NA` values occurring in the data beforehand.
#' @param x an epi_df to be filled forward.
#' @param columns_to_complete which columns to apply completion to. By default every non-key column of an epi_df
#' @param groups the grouping by which to fill forward
#' @importFrom tidyselect all_of
#' @importFrom rlang list2
#' @importFrom vctrs vec_cast
#' @importFrom dplyr across arrange bind_rows group_by summarise
#' @keywords internal
pad_to_end <- function(x, groups, end_date, columns_to_complete = NULL) {
  if (is.null(columns_to_complete)) {
    columns_to_complete <- setdiff(names(x), key_colnames(x))
  }
  itval <- epiprocess:::guess_period(c(x$time_value, end_date), "time_value")
  # get the time values we need to fill in
  completed_time_values <-
    x %>%
    group_by(across(all_of(groups))) %>%
    summarise(
      time_value = list2(
        time_value = seq_null_swap(from = max(time_value) + itval, to = end_date, by = itval)
      )
    ) %>%
    unnest("time_value") %>%
    mutate(time_value = vec_cast(time_value, x$time_value))
  # pull the last value in each group and fill forward
  grouped_and_arranged <- x %>%
    arrange(across(all_of(c("time_value", groups)))) %>%
    group_by(across(all_of(groups)))
  values_to_fill <- grouped_and_arranged %>%
    slice(min(across(all_of(columns_to_complete), count_single_column)):n())
  filled_values <- values_to_fill %>%
    bind_rows(completed_time_values) %>%
    arrange(across(all_of(c("time_value", groups)))) %>%
    fill(all_of(columns_to_complete), .direction = "down") %>%
    slice(-1) # remove the oirginal rows

  grouped_and_arranged %>%
    slice(1:min(across(all_of(columns_to_complete), count_single_column))) %>%
    bind_rows(filled_values) %>%
    arrange(across(all_of(key_colnames(x)))) %>%
    ungroup()
}

#' get the location of the last real value
#' @param col the relevant column
#' @keywords internal
count_single_column <- function(col) {
  max(which(!is.na(col)))
}


#' seq, but returns null if from is larger
#' @keywords internal
seq_null_swap <- function(from, to, by) {
  if (from > to) {
    return(NULL)
  }
  seq(from = from, to = to, by = by)
}


#' warn when the latency is larger than would be reasonable
#' @param dataset the epi_df
#' @param latency_table the whole collection of latencies
#' @param target_columns the names of the columns that we're adjusting, and whether its unreasonably latent
#' @keywords internal
check_interminable_latency <- function(dataset, latency_table, target_columns, forecast_date, call = caller_env()) {
  # check that the shift amount isn't too extreme
  rel_latency_table <- latency_table %>%
    filter(col_name %in% target_columns)
  # no relevant columns, so this error definitely isn't happening
  if (nrow(rel_latency_table) == 0) {
    return()
  }
  latency_max <- rel_latency_table %>%
    pull(latency) %>%
    abs() %>%
    max()
  time_type <- attributes(dataset)$metadata$time_type
  i_latency <- which.max(latency_table$latency)
  if (
    (grepl("day", time_type) && (latency_max >= 28)) ||
      (grepl("week", time_type) && (latency_max >= 4)) ||
      ((time_type == "yearmonth") && (latency_max >= 2)) ||
      ((time_type == "yearquarter") && (latency_max >= 1)) ||
      ((time_type == "year") && (latency_max >= 1))
  ) {
    max_time_value <- dataset %>%
      filter(!is.na(!!(latency_table[[i_latency, "col_name"]]))) %>%
      pull(time_value) %>%
      max()
    cli_warn(
      message = c(
        paste(
          "The maximum latency is {latency_max}, ",
          "which is questionable for it's `time_type` of ",
          "{time_type}."
        ),
        "i" = "latency: {latency_table$latency[[i_latency]]}",
        "i" = "`max_time` = {max_time_value} -> `forecast_date` = {forecast_date}"
      ),
      class = "epipredict__prep.step_latency__very_large_latency",
      call = call
    )
  }
}

`%nin%` <- function(x, table) {
  !(x %in% table)
}

#' create the latency table
#' This is a table of column names and the latency adjustment necessary for that column. An example:
#'
#'   col_name   latency
#'   <chr>        <int>
#' 1 case_rate        5
#' 2 death_rate       5
#' @keywords internal
#' @importFrom dplyr rowwise
get_latency_table <- function(training, columns, forecast_date, latency,
                              sign_shift, epi_keys_checked, info, terms) {
  if (is.null(columns)) {
    columns <- recipes_eval_select(terms, training, info)
  }
  # construct the latency table
  latency_table <- tibble(col_name = names(training)) %>%
    filter(col_name %nin% key_colnames(training))
  if (length(columns) > 0) {
    latency_table <- latency_table %>% filter(col_name %in% columns)
  }

  if (is.null(latency)) {
    latency_table <- latency_table %>%
      rowwise() %>%
      mutate(latency = get_latency(
        training, forecast_date, col_name, sign_shift, epi_keys_checked
      ))
  } else if (length(latency) > 1) {
    # if latency has a length, it must also have named elements.
    # We assign based on comparing the name in the list
    # with the column names, and drop any which don't have a latency assigned
    latency_table <- latency_table %>%
      filter(col_name %in% names(latency)) %>%
      rowwise() %>%
      mutate(latency = unname(latency[names(latency) == col_name]))
  } else {
    latency_table <- latency_table %>%
      rowwise() %>%
      mutate(latency = get_latency(
        training, forecast_date, col_name, sign_shift, epi_keys_checked
      ))
    if (latency) {
      latency_table <- latency_table %>% mutate(latency = latency)
    }
  }
  return(latency_table %>% ungroup())
}


#' checks: the recipe type, whether a previous step is the relevant epi_shift,
#' that either `fixed_latency` or `fixed_forecast_date` is non-null, and that
#' `fixed_latency` only references columns that exist at the time of the step
#' inclusion
#' @keywords internal
step_adjust_latency_checks <- function(id, method, recipe, fixed_latency, fixed_forecast_date, call = caller_env()) {
  arg_is_chr_scalar(id, method)
  if (detect_step(recipe, "adjust_latency")) {
    cli_abort("Only one `step_adjust_latency()` can be included in a recipe.",
      class = "epipredict__step_adjust_latency__multiple_steps"
    )
  }
  if (!is_epi_recipe(recipe)) {
    cli_abort("This recipe step can only operate on an {.cls epi_recipe}.",
      class = "epipredict__step_adjust_latency__epi_recipe_only"
    )
  }
  if ((method == "extend_ahead") && (detect_step(recipe, "epi_ahead"))) {
    cli_warn(
      "If `method` is {.val extend_ahead}, then the previous `step_epi_ahead` won't be modified.",
      class = "epipredict__step_adjust_latency__misordered_step_warning"
    )
  } else if ((method == "extend_lags") && detect_step(recipe, "epi_lag")) {
    cli_warn(
      "If `method` is {.val extend_lags} or {.val locf},
then the previous `step_epi_lag`s won't work with modified data.",
      class = "epipredict__step_adjust_latency__misordered_step_warning"
    )
  } else if ((method == "locf") && (length(recipe$steps) > 0)) {
    cli_warn(
      paste0(
        "There are steps before `step_adjust_latency`.",
        " With the method {.val locf}, it is recommended to include this step before any others"
      ),
      class = "epipredict__step_adjust_latency__misordered_step_warning"
    )
  }
  if (!is.null(fixed_latency) && !is.null(fixed_forecast_date)) {
    cli_abort(
      "Only one of `fixed_latency` and `fixed_forecast_date` can be non-`NULL` at a time!",
      class = "epipredict__step_adjust_latency__too_many_args_error"
    )
  }
  if (length(fixed_latency > 1)) {
    template <- recipe$template
    data_names <- names(template)[!names(template) %in% key_colnames(template)]
    wrong_names <- names(fixed_latency)[!names(fixed_latency) %in% data_names]
    if (length(wrong_names) > 0) {
      cli_abort(
        "{.val fixed_latency} contains names not in the template dataset: {wrong_names}",
        class = "epipredict__step_adjust_latency__undefined_names_error"
      )
    }
  }
}
