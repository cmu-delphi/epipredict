#' Get test data for prediction based on longest lag period
#'
#' Based on the longest lag period in the recipe,
#' `get_test_data()` creates a tibble in [epiprocess::epi_df]
#' format with columns `geo_value`, `time_value`
#' and other variables in the original dataset,
#' which will be used to create test data.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param x A data frame, tibble, or epi_df data set.
#'
#' @return A tibble with columns `geo_value`, `time_value`
#' and other variables in the original dataset.
#' @examples
#' # create recipe
#'  rec <- epi_recipe(case_death_rate_subset) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14))
#'  get_test_data(recipe = rec, x = case_death_rate_subset)
#' @export

get_test_data <- function(recipe, x){
  # TO-DO: SOME CHECKS OF THE DATASET
  if (any(!(c('geo_value','time_value') %in% colnames(x)))) {
    rlang::abort("`geo_value`, `time_value` does not exist in data")
  }
  ## CHECK if it is epi_df?


  # initialize vector to hold max lags for each variable
  max_lags <- c()
  for (i in seq_along(recipe$steps)) {
    if ("lag" %in% names(recipe$steps[[i]])) {
      max_lags <- append(max_lags, max(recipe$steps[[i]]$lag))
    }
  }

  # CHECK: Return NA if insufficient training data
  if (dplyr::n_distinct(x$time_value) < max(max_lags)) {
    stop("insufficient training data")
  }

  test_data <- x %>%
    dplyr::filter(
      dplyr::if_any(
        .cols = recipe$term_info$variable[which(recipe$var_info$role == 'raw')],
        .fns = ~ !is.na(.x)
      )
    ) %>%
    epiprocess::group_by(geo_value) %>%
    dplyr::slice_tail(n = max(max_lags) + 1) %>%
    epiprocess::ungroup()

  return(test_data)
}
