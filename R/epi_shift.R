#' Shift predictors while maintaining grouping and time_value ordering
#'
#' This is a lower-level function. As such it performs no error checking.
#'
#' @param x Data frame.
#' @param shift_val a single integer. Negative values produce leads.
#' @param newname the name for the newly shifted column
#' @param key_cols vector, or `NULL`. Additional grouping vars.
#'
#' @keywords internal
#'
#' @return a list of tibbles
epi_shift_single <- function(x, col, shift_val, newname, key_cols) {
  x %>%
    select(all_of(c(key_cols, col))) %>%
    mutate(time_value = time_value + shift_val) %>%
    rename(!!newname := {{ col }})
}

#' lags move columns forward to bring the past up to today, while aheads drag
#' the future back to today
#' @keywords internal
get_sign <- function(object) {
  if (!is.null(object$prefix)) {
    if (object$prefix == "lag_") {
      return(1)
    } else {
      return(-1)
    }
  } else if (object$method == "extend_lags") {
    return(1)
  } else {
    return(-1)
  }
}

#' backend for both `bake.step_epi_ahead` and `bake.step_epi_lag`, performs the
#' checks missing in `epi_shift_single`
#' @keywords internal
#' @importFrom tidyr expand_grid
#' @importFrom dplyr join_by
add_shifted_columns <- function(new_data, object) {
  grid <- object$shift_grid

  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% grid$newname
  if (any(intersection)) {
    cli_abort(c(
      "Name collision occured in {.cls {class(object)[1]}}",
      "The following variable name{?s} already exist{?s/}: {.val {new_data_names[intersection]}}."
    ))
  }
  ok <- object$keys
  shifted <- reduce(
    pmap(grid, epi_shift_single, x = new_data, key_cols = ok),
    full_join,
    by = ok
  )
  processed <- new_data %>%
    full_join(shifted, by = ok) %>%
    group_by(across(all_of(kill_time_value(ok)))) %>%
    arrange(time_value) %>%
    ungroup() %>%
    as_epi_df()
  attributes(processed)$metadata <- attributes(new_data)$metadata
  return(processed)
}
