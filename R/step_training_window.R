#' Limits the size of the training window to the most recent observations
#'
#' `step_training_window` creates a *specification* of a recipe step that
#'   limits the size of the training window to the `n_recent` most recent
#'   observations in `time_value` per group, where the groups are formed
#'   based on the remaining `epi_keys`.
#'
#' @param n_recent An integer value that represents the number of most recent
#'   observations that are to be kept in the training window per key
#'   The default value is 50.
#' @param seasonal Bool, default FALSE. If TRUE, the training window will slice
#'   through epidemic seasons. This is useful for forecasting models that need
#'   to leverage the data in previous years, but only limited to similar phases
#'   in the epidemic. Most useful to heavily seasonal data, like influenza.
#'   Expects n_recent to be finite.
#' @param seasonal_forward_window An integer value that represents the number of days
#'   after a season week to include in the training window. The default value
#'   is 14. Only valid when seasonal is TRUE.
#' @param seasonal_backward_window An integer value that represents the number of days
#'   before a season week to include in the training window. The default value
#'   is 35. Only valid when seasonal is TRUE.
#' @param epi_keys An optional character vector for specifying "key" variables
#'   to group on. The default, `NULL`, ensures that every key combination is
#'   limited.
#' @inheritParams step_epi_lag
#' @template step-return
#'
#' @details Note that `step_epi_lead()` and `step_epi_lag()` should come
#' after any filtering step.
#'
#' @export
#'
#' @examples
#' tib <- tibble(
#'   x = 1:10,
#'   y = 1:10,
#'   time_value = rep(seq(as.Date("2020-01-01"), by = 1, length.out = 5), 2),
#'   geo_value = rep(c("ca", "hi"), each = 5)
#' ) %>%
#'   as_epi_df()
#'
#' epi_recipe(y ~ x, data = tib) %>%
#'   step_training_window(n_recent = 3) %>%
#'   prep(tib) %>%
#'   bake(new_data = NULL)
#'
#' epi_recipe(y ~ x, data = tib) %>%
#'   step_epi_naomit() %>%
#'   step_training_window(n_recent = 3) %>%
#'   prep(tib) %>%
#'   bake(new_data = NULL)
step_training_window <-
  function(recipe,
           role = NA,
           n_recent = 50,
           seasonal = FALSE,
           seasonal_forward_window = 14,
           seasonal_backward_window = 35,
           epi_keys = NULL,
           id = rand_id("training_window")) {
    arg_is_scalar(n_recent, id, seasonal, seasonal_forward_window, seasonal_backward_window)
    arg_is_pos(n_recent, seasonal_forward_window, seasonal_backward_window)
    if (is.finite(n_recent)) arg_is_pos_int(n_recent)
    arg_is_chr(id)
    arg_is_chr(epi_keys, allow_null = TRUE)
    add_step(
      recipe,
      step_training_window_new(
        role = role,
        trained = FALSE,
        n_recent = n_recent,
        seasonal = seasonal,
        seasonal_forward_window = seasonal_forward_window,
        seasonal_backward_window = seasonal_backward_window,
        epi_keys = epi_keys,
        skip = TRUE,
        id = id
      )
    )
  }

step_training_window_new <-
  function(role, trained, n_recent, seasonal, seasonal_forward_window, seasonal_backward_window, epi_keys, skip, id) {
    step(
      subclass = "training_window",
      role = role,
      trained = trained,
      n_recent = n_recent,
      seasonal = seasonal,
      seasonal_forward_window = seasonal_forward_window,
      seasonal_backward_window = seasonal_backward_window,
      epi_keys = epi_keys,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_training_window <- function(x, training, info = NULL, ...) {
  ekt <- epi_keys_only(training)
  ek <- x$epi_keys %||% ekt %||% character(0L)

  hardhat::validate_column_names(training, ek)

  step_training_window_new(
    role = x$role,
    trained = TRUE,
    n_recent = x$n_recent,
    seasonal = x$seasonal,
    seasonal_forward_window = x$seasonal_forward_window,
    seasonal_backward_window = x$seasonal_backward_window,
    epi_keys = ek,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_training_window <- function(object, new_data, ...) {
  hardhat::validate_column_names(new_data, object$epi_keys)

  if (object$n_recent < Inf) {
    new_data <- new_data %>%
      group_by(across(all_of(object$epi_keys))) %>%
      arrange(time_value) %>%
      dplyr::slice_tail(n = object$n_recent) %>%
      ungroup()
  }

  if (object$seasonal) {
    # TODO: This needs to take into account different time types of time_value.
    # Currently, it assumes time_value is a Date.
    new_data <- new_data %>% add_season_info()
    last_data_season_week <- new_data %>%
      filter(time_value == max(time_value)) %>%
      pull(season_week) %>%
      max()
    recent_weeks <- c(last_data_season_week)
    if (inherits(new_data, "epi_df")) {
      current_season_week <- convert_epiweek_to_season_week(epiyear(epi_as_of(new_data)), epiweek(epi_as_of(new_data)))
      recent_weeks <- c(recent_weeks, current_season_week)
    }
    date_ranges <- new_data %>%
      filter(season_week %in% recent_weeks) %>%
      pull(time_value) %>%
      unique() %>%
      map(~ c(.x - 1:(object$seasonal_backward_window), .x + 0:(object$seasonal_forward_window))) %>%
      unlist() %>%
      as.Date() %>%
      unique()
    new_data %<>% filter(time_value %in% date_ranges)
  }


  new_data
}

#' @export
print.step_training_window <-
  function(x, width = max(20, options()$width - 30), ...) {
    if (x$seasonal) {
      title <- "# of seasonal observations per key limited to:"
      n_recent <- x$n_recent
      seasonal_forward_window <- x$seasonal_forward_window
      seasonal_backward_window <- x$seasonal_backward_window
      tr_obj <- recipes::format_selectors(rlang::enquos(n_recent, seasonal_forward_window, seasonal_backward_window), width)
      recipes::print_step(tr_obj, rlang::enquos(n_recent, seasonal_forward_window, seasonal_backward_window), x$trained, title, width)
    } else {
      title <- "# of recent observations per key limited to:"
      n_recent <- x$n_recent
      tr_obj <- recipes::format_selectors(rlang::enquos(n_recent), width)
      recipes::print_step(tr_obj, rlang::enquos(n_recent), x$trained, title, width)
    }
    invisible(x)
  }
