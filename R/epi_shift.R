#' Shift predictors while maintaining grouping and time_value ordering
#'
#' This is a lower-level function. As such it performs no error checking.
#'
#' @param x Data frame. Variables to lag
#' @param lags List. Each list element is a vector of lags.
#'   Negative values produce leads. The list should have the same
#'   length as the number of columns in `x`.
#' @param time_value Vector. Same length as `x` giving time stamps.
#' @param keys Data frame, vector, or `NULL`. Additional grouping vars.
#' @param out_name Chr. The output list will use this as a prefix.
#'
#' @return a list of tibbles
epi_shift <- function(x, lags, time_value, keys = NULL, out_name = "x") {
  if (!is.data.frame(x)) x <- data.frame(x)
  if (is.null(keys)) keys <- rep("empty", nrow(x))
  p_in = ncol(x)
  out_list <- tibble::tibble(i = 1:p_in, lag = lags) %>%
    tidyr::unchop(lag) %>% # what is chop
    dplyr::mutate(name = paste0(out_name, 1:nrow(.))) %>%
    # One list element for each lagged feature
    purrr::pmap(function(i, lag, name) {
      tibble(keys,
             time_value = time_value + lag, # Shift back
             !!name := x[[i]])
    })
  if (is.data.frame(keys)) common_names <- c(names(keys), "time_value")
  else common_names <- c("keys", "time_value")

  purrr::reduce(out_list, dplyr::full_join, by = common_names)
}
