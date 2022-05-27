#' Create a lagged predictor
#'
#' `step_epi_shift` creates a *specification* of a recipe step that
#'   will add new columns of lagged data. Lagged data will
#'   by default include NA values where the lag was induced.
#'   These can be removed with [step_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param shift A vector of integers. Each specified column will be
#'  lagged for each value in the vector.
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for lagging.
#'
#' @family row operation steps
#' @export
#' @rdname step_epi_ahead
step_epi_shift2 <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           shift2 = 1,
           prefix = "shift2_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_shift2")) {
    add_step(
      recipe,
      step_epi_shift2_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        shift2 = shift2,
        prefix = prefix,
        default = default,
        keys = keys,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_epi_shift2_new <-
  function(terms, role, trained, shift2, prefix, default, keys,
           columns, skip, id) {
    step(
      subclass = "epi_shift2",
      terms = terms,
      role = role,
      trained = trained,
      shift2 = shift2,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_epi_shift2 <- function(x, training, info = NULL, ...) {
  step_epi_shift2_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    shift2 = x$shift2,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_shift2 <- function(object, new_data, ...) {
  if (!all(object$shift2 == as.integer(object$shift2))) {
    rlang::abort("step_epi_shift2 requires 'shift2' argument to be integer valued.")
  }
  grid <- tidyr::expand_grid(col = object$columns, shift2_val = object$shift2) %>%
    dplyr::mutate(newname = glue::glue("{object$prefix}{shift2_val}_{col}"))

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
  shiftged <- purrr::reduce(
    purrr::pmap(grid, epi_shift_single, x = new_data, key_cols = ok),
    dplyr::full_join,
    by = ok
  )

  dplyr::full_join(new_data, shifted, by = ok) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::ungroup()

}

#' @export
print.step_epi_shift2 <-
  function(x, width = max(20, options()$width - 30), ...) {
    ## TODO add printing of the lags
    title <- "Lagging "
    recipes::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }
