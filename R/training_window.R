#' Limits the size of the training window to the most recent observations
#'
#' `step_training_window` creates a *specification* of a recipe step that
#'   limit the size of the training window to the `nrec` most recent
#'   observations in `time_value` per location from `geo_value`.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param nrec An integer value that represents the number of most recent
#' observations that are to be kept in the training window per location.
#' The default value is 50.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @template step-return
#'
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
#'   step_training_window(nrec = 3) %>%
#'   prep(tib) %>%
#'   bake(new_data = NULL)
step_training_window <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           nrec = 50,
           skip = TRUE,
           id = rand_id("training_window")) {

    add_step(
      recipe,
      step_training_window_new(
        role = role,
        trained = trained,
        nrec = nrec,
        skip = skip,
        id = id
      )
    )
  }

step_training_window_new <-
  function(terms, role, trained, nrec, skip, id) {
    step(
      subclass = "training_window",
      role = role,
      trained = trained,
      nrec = nrec,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_training_window <- function(x, training, info = NULL, ...) {

  step_training_window_new(
    role = x$role,
    trained = TRUE,
    nrec = x$nrec,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_training_window <- function(object, new_data, ...) {
  if (!all(object$nrec == as.integer(object$nrec))) {
    rlang::abort("step_training_window requires 'nrec' to be integer valued.")
  }

  new_data %>% dplyr::group_by(geo_value) %>%
    dplyr::arrange(time_value) %>%
    dplyr::slice_tail(n = object$nrec) %>%
    dplyr::ungroup()
}

#' @export
print.step_training_window <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Number of most recent observations per location used in training window "
    nrec = x$nrec
    tr_obj = format_selectors(enquos(nrec), width)
    recipes::print_step(tr_obj, enquos(nrec),
                        x$trained, title, width)
    invisible(x)
  }
