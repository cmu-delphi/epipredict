#' Get test data for prediction based on longest lag period
#'
#' Based on the longest lag period in the recipe,
#' `get_test_data()` creates an [epiprocess::epi_df]
#' with columns `geo_value`, `time_value`
#' and other variables in the original dataset,
#' which will be used to create test data.
#'
#' It also optionally fills missing values
#' using the last-observation-carried-forward (LOCF) method. If this
#' is not possible (say because there would be only `NA`'s in some location),
#' it will produce an error suggesting alternative options to handle missing
#' values with more advanced techniques.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param x A data frame, tibble, or epi_df data set.
#' @param fill_locf Logical. Should we use `locf` to fill in missing data?
#' @param n_recent Integer or NULL. If filling missing data with `locf=TRUE`,
#'   how far back are we willing to tolerate missing data? Larger values allow
#'   more filling. The default `NULL` will determine this from the maximum
#'   lags used in the `recipe`. For example, suppose n_recent = 3, then if the
#'   3 most recent observations in some region are all `NA`’s, we won’t be able
#'   to fill anything, and an error message will be thrown.
#'
#' @return A tibble with columns `geo_value`, `time_value`, any additional
#'   keys, as well other variables in the original dataset.
#' @examples
#' # create recipe
#' rec <- epi_recipe(case_death_rate_subset) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14))
#' get_test_data(recipe = rec, x = case_death_rate_subset)
#' @importFrom rlang `%@%`
#' @export

get_test_data <- function(
    recipe,
    x,
    fill_locf = FALSE,
    n_recent = NULL,
    forecast_date = max(x$time_value)
) {
  stopifnot(is.data.frame(x))
  arg_is_lgl(fill_locf)
  arg_is_scalar(fill_locf)
  arg_is_pos_int(n_recent, allow_null = TRUE)
  arg_is_scalar(n_recent, allow_null = TRUE)

  if (!all(colnames(recipe$template) %in% colnames(x)))
    cli_stop("some variables used for training are not available in `x`.")
  min_lags <- min(map_dbl(recipe$steps, ~ min(.x$lag %||% Inf)), Inf)
  max_lags <- max(map_dbl(recipe$steps, ~ max(.x$lag %||% 0)), 0)
  max_horizon <- max(map_dbl(recipe$steps, ~ max(.x$horizon %||% 0)), 0)
  min_required <- max_lags + max_horizon
  if (is.null(n_recent)) n_recent <- min_required + 1

  # CHECK: Error out if insufficient training data
  # Probably needs a fix based on the time_type of the epi_df
  if (diff(range(x$time_value)) < min_required) {
    time_type <- (recipe$template %@% "metadata")$time_type
    cli_stop("You supplied insufficient training data for this recipe. ",
             i = "You need at least {min_required} days of data.")
  }

  groups <- kill_time_value(epi_keys(recipe))

  # If we skip NA completion, we remove undesireably early time values
  # Happens globally, over all groups
  keep <- max(n_recent, min_required + 1)
  x <- dplyr::filter(x, forecast_date - time_value <= keep)

  # If all(lags > 0), then we get rid of recent data
  if (min_lags > 0 && min_lags < Inf)
    x <- dplyr::filter(x, forecast_date - time_value < min_lags)

  # Pad with explicit missing values up to and including the forecast_date
  # x is grouped here
  x <- pad_to_end(x, groups, forecast_date) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(groups)))


  # Now, fill forward missing data if requested
  if (fill_locf) {
    cannot_be_used <- x %>%
      dplyr::filter(forecast_date - time_value <= n_recent) %>%
      dplyr::summarize(
        dplyr::across(!time_value, ~ !is.na(.x[1])), .groups = "drop") %>%
      dplyr::summarise(dplyr::across(-dplyr::all_of(groups), ~ any(!.x))) %>%
      unlist()
    if (any(cannot_be_used)) {
      bad_vars <- names(cannot_be_used)[cannot_be_used]
      cli_stop(
        "The variables {bad_vars} have ",
        "too many recent missing values to be filled automatically. ",
        "You should either choose `n_recent` larger than its current ",
        "value {n_recent}, or perform NA imputation manually, perhaps with ",
        "{.code recipes::step_impute_*()} or with {.code tidyr::fill()}."
      )
    }
    x <- tidyr::fill(x, !time_value)
  }

  dplyr::filter(x, forecast_date - time_value <= min_required) %>%
    epiprocess::ungroup()
}

pad_to_end <- function(x, groups, end_date) {
  itval <- epiprocess:::guess_period(c(x$time_value, end_date), "time_value")
  completed_time_values <- x %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(groups))) %>%
    dplyr::summarise(
      time_value = rlang::list2(
        time_value = Seq(max(time_value) + itval, end_date, itval)
      )
    ) %>%
    unnest("time_value") %>%
    mutate(time_value = vctrs::vec_cast(time_value, x$time_value))

  vctrs::vec_rbind(x, completed_time_values) %>%
    dplyr::arrange(dplyr::across(tidyselect::all_of(c("time_value", groups))))
}

Seq <- function(from, to, by) {
  if (from > to) return(NULL)
  seq(from = from, to = to, by = by)
}
