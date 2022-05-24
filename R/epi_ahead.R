#' Create a leading outcome
#'
#' `step_epi_ahead` creates a *specification* of a recipe step that
#'   will add new columns of leading data. Leading data will
#'   by default include NA values where the lag was induced.
#'   These can be removed with [step_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param ahead A vector of positive integers. Each specified column will be
#'  lead for each value in the vector.
#' @param prefix A prefix for generated column names, default to "ahead_".
#' @param default Determines what fills empty rows
#'   left by leading/lagging (defaults to NA).
#' @param keys A character vector of the keys in an epi_df
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A character string that is unique to this step to identify it.
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for leading.
#'
#' @family row operation steps
#' @export
#'
#' @examples
#' tib <- tibble::tibble(
#'   x = 1:5, y = 1:5,
#'   time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
#'   geo_value = "ca"
#'   ) %>% epiprocess::as_epi_df()
#'
#' library(recipes)
#' epi_recipe(y ~ x, data = tib) %>%
#'   step_epi_lag(x, lag = 2:3) %>%
#'   step_epi_ahead(y, ahead = 1) %>%
#'   prep(tib) %>%
#'   bake(tib)
step_epi_ahead <-
  function(recipe,
           ...,
           role = "outcome",
           trained = FALSE,
           ahead = 1,
           prefix = "ahead_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_ahead")) {
    add_step(
      recipe,
      step_epi_ahead_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        ahead = ahead,
        prefix = prefix,
        default = default,
        keys = keys,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_epi_ahead_new <-
  function(terms, role, trained, ahead, prefix, default, keys,
           columns, skip, id) {
    step(
      subclass = "epi_ahead",
      terms = terms,
      role = role,
      trained = trained,
      ahead = ahead,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_epi_ahead <- function(x, training, info = NULL, ...) {
  step_epi_ahead_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    ahead = x$ahead,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_ahead <- function(object, new_data, ...) {
  if (!all(object$ahead == as.integer(object$ahead))) {
    rlang::abort("step_epi_ahead requires 'ahead' argument to be integer valued.")
  }

  grid <- tidyr::expand_grid(
    col = object$columns, lag_val = -object$ahead) %>%
    dplyr::mutate(
      ahead_val = -lag_val,
      newname = glue::glue("{object$prefix}{ahead_val}_{col}")
    ) %>%
    dplyr::select(-ahead_val)

  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% grid$newname
  if (any(intersection)) {
    rlang::abort(
      paste0("Name collision occured in `", class(object)[1],
             "`. The following variable names already exists: ",
             paste0(new_data_names[intersection], collapse = ", "),
             "."))
  }

  ok <- object$keys
  lagged <- purrr::reduce(
    purrr::pmap(grid, epi_shift_single, x = new_data, key_cols = ok),
    dplyr::full_join,
    by = ok
  )

  dplyr::full_join(new_data, lagged, by = ok) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::ungroup()

}

#' @export
print.step_epi_ahead <-
  function(x, width = max(20, options()$width - 30), ...) {
    ## TODO add printing of the lags
    title <- "Leading "
    recipes::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }
