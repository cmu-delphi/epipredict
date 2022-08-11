#' Shift predictors while maintaining grouping and time_value ordering
#'
#' This is a lower-level function. As such it performs no error checking.
#'
#' @param x Data frame. Variables to shift
#' @param shifts List. Each list element is a vector of shifts.
#'   Negative values produce leads. The list should have the same
#'   length as the number of columns in `x`.
#' @param time_value Vector. Same length as `x` giving time stamps.
#' @param keys Data frame, vector, or `NULL`. Additional grouping vars.
#' @param out_name Chr. The output list will use this as a prefix.
#'
#' @return a list of tibbles
epi_shift <- function(x, shifts, time_value, keys = NULL, out_name = "x") {
  if (!is.data.frame(x)) x <- data.frame(x)
  if (is.null(keys)) keys <- rep("empty", nrow(x))
  p_in = ncol(x)
  out_list <- tibble::tibble(i = 1:p_in, shift = shifts) %>%
    tidyr::unchop(shift) %>% # what is chop
    dplyr::mutate(name = paste0(out_name, 1:nrow(.))) %>%
    # One list element for each shifted feature
    pmap(function(i, shift, name) {
      tibble(keys,
             time_value = time_value + shift, # Shift back
             !!name := x[[i]])
    })
  if (is.data.frame(keys)) common_names <- c(names(keys), "time_value")
  else common_names <- c("keys", "time_value")

  reduce(out_list, dplyr::full_join, by = common_names)
}

epi_shift_single <- function(x, col, shift_val, newname, key_cols) {
  x %>%
    dplyr::select(tidyselect::all_of(c(key_cols, col))) %>%
    dplyr::mutate(time_value = time_value + shift_val) %>%
    dplyr::rename(!!newname := col)
}
