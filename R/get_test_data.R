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

  max_lags <- max(map_dbl(recipe$steps, ~ max(.x$lag %||% 0)))

  # CHECK: Return NA if insufficient training data
  if (dplyr::n_distinct(x$time_value) < max_lags) {
    cli_stop("You supplied insufficient training data for this recipe. ",
             "You need at least {max_lags} distinct time_values.")
  }

  groups <- epi_keys(recipe)[epi_keys(recipe) != "time_value"]

  x %>%
    dplyr::filter(
      dplyr::if_any(
        .cols = recipe$term_info$variable[which(recipe$var_info$role == 'raw')],
        .fns = ~ !is.na(.x)
      )
    ) %>%
    epiprocess::group_by(dplyr::across(dplyr::all_of(groups))) %>%
    dplyr::slice_tail(n = max_lags + 1) %>%
    epiprocess::ungroup()

}
