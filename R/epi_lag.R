#' Create a lagged predictor
#'
#' `step_epi_lag` creates a *specification* of a recipe step that
#'   will add new columns of lagged data. Lagged data will
#'   by default include NA values where the lag was induced.
#'   These can be removed with [step_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param lag A vector of positive integers. Each specified column will be
#'  lagged for each value in the vector.
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for lagging.
#'
#' @family row operation steps
#' @export
#' @rdname step_epi_lag
step_epi_lag <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           lag = 1, # negative for ahead
           prefix = "lag_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_lag")) {
    add_step(
      recipe,
      step_epi_lag_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        lag = lag,
        prefix = prefix,
        default = default,
        keys = keys,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_epi_lag_new <-
  function(terms, role, trained, lag, prefix, default, keys,
           columns, skip, id) {
    step(
      subclass = "epi_lag",
      terms = terms,
      role = role,
      trained = trained,
      lag = lag,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_epi_lag <- function(x, training, info = NULL, ...) {
  step_epi_lag_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lag = x$lag,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_lag <- function(object, new_data, ...) {
  if (!all(object$lag == as.integer(object$lag))) {
    rlang::abort("step_epi_lag requires 'lag' argument to be integer valued.")
  }

  grid <- tidyr::expand_grid(col = object$columns, lag_val = object$lag) %>%
    dplyr::mutate(newname = glue::glue("{object$prefix}{lag_val}_{col}"))

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
print.step_epi_lag <-
  function(x, width = max(20, options()$width - 30), ...) {
    ## TODO add printing of the lags
    title <- ifelse(x$lag >= 0, "Lagging", "Leading")
    recipes::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }
