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
           epi_keys = NULL,
           id = rand_id("training_window")) {
    arg_is_scalar(n_recent, id)
    arg_is_pos(n_recent)
    if (is.finite(n_recent)) arg_is_pos_int(n_recent)
    arg_is_chr(id)
    arg_is_chr(epi_keys, allow_null = TRUE)
    add_step(
      recipe,
      step_training_window_new(
        role = role,
        trained = FALSE,
        n_recent = n_recent,
        epi_keys = epi_keys,
        skip = TRUE,
        id = id
      )
    )
  }

step_training_window_new <-
  function(role, trained, n_recent, epi_keys, skip, id) {
    step(
      subclass = "training_window",
      role = role,
      trained = trained,
      n_recent = n_recent,
      epi_keys = epi_keys,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_training_window <- function(x, training, info = NULL, ...) {
  ekt <- kill_time_value(key_colnames(training))
  ek <- x$epi_keys %||% ekt %||% character(0L)

  hardhat::validate_column_names(training, ek)

  step_training_window_new(
    role = x$role,
    trained = TRUE,
    n_recent = x$n_recent,
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

  new_data
}

#' @export
print.step_training_window <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "# of recent observations per key limited to:"
    n_recent <- x$n_recent
    tr_obj <- recipes::format_selectors(rlang::enquos(n_recent), width)
    recipes::print_step(tr_obj, rlang::enquos(n_recent), x$trained, title, width)
    invisible(x)
  }
