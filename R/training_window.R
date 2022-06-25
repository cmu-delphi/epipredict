#' Limits the size of the training window to the most recent observations
#'
#' `step_training_window` creates a *specification* of a recipe step that
#'   limit the size of the training window to the `n_recent` most recent
#'   observations in `time_value` per group, where the groups are formed
#'   based on the remaining `epi_keys`.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param role Not used by this step since no new variables are created.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param n_recent An integer value that represents the number of most recent
#' observations that are to be kept in the training window per location.
#' The default value is 50.
#' @param id A character string that is unique to this step to identify it.
#' @template step-return
#'
#' @details Note that `step_epi_lead()` and `step_epi_lag()` should come
#' after any filtering step.
#'
#' @export
#'
#' @examples
#' tib <- tibble::tibble(
#' x = 1:10, y = 1:10,
#' time_value = rep(seq(as.Date("2020-01-01"), by = 1,
#'                      length.out = 5), times = 2),
#' geo_value = rep(c("ca", "hi"), each = 5)
#' ) %>% epiprocess::as_epi_df()
#'
#' library(recipes)
#' epi_recipe(y ~ x, data = tib) %>%
#'   step_training_window(n_recent = 3) %>%
#'   prep(tib) %>%
#'   bake(new_data = NULL)
step_training_window <-
  function(recipe,
           role = NA,
           trained = FALSE,
           n_recent = 50,
           id = rand_id("training_window")) {

    add_step(
      recipe,
      step_training_window_new(
        role = role,
        trained = trained,
        n_recent = n_recent,
        skip = TRUE,
        id = id
      )
    )
  }

step_training_window_new <-
  function(terms, role, trained, n_recent, skip, id = id) {
    step(
      subclass = "training_window",
      role = role,
      trained = trained,
      n_recent = n_recent,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_training_window <- function(x, training, info = NULL) {

  step_training_window_new(
    role = x$role,
    trained = TRUE,
    n_recent = x$n_recent,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_training_window <- function(object, new_data) {
  if (!all(object$n_recent == as.integer(object$n_recent))) {
    rlang::abort("step_training_window requires 'n_recent' to be integer valued.")
  }

  ek <- epi_keys(new_data)[which(epi_keys(new_data) != "time_value")]

  new_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ek))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::slice_tail(n = object$n_recent) %>%
    dplyr::ungroup()
}

#' @export
print.step_training_window <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Number of most recent observations per location used in training window "
    n_recent = x$n_recent
    tr_obj = format_selectors(rlang::enquos(n_recent), width)
    recipes::print_step(tr_obj, rlang::enquos(n_recent),
                        x$trained, title, width)
    invisible(x)
  }
