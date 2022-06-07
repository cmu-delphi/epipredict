#' Create a shifted predictor
#'
#' `step_epi_shift` creates a *specification* of a recipe step that
#'   will add new columns of shifted data. shifted data will
#'   by default include NA values where the shift was induced.
#'   These can be removed with [step_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param shift A vector of integers. Each specified column will be
#'  shifted for each value in the vector.
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for shifting.
#'
#' @family row operation steps
#' @rdname step_epi_ahead
#' @export
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
    step_epi_shift(recipe,
                   ...,
                   role = role,
                   trained = trained,
                   shift = ahead,
                   prefix = prefix,
                   default = default,
                   keys = keys,
                   columns = columns,
                   skip = skip,
                   id = id
    )
  }

#' @export
step_epi_lag <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           lag = 1,
           prefix = "lag_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_lag")) {
    step_epi_shift(recipe,
                   ...,
                   role = role,
                   trained = trained,
                   shift = -lag,
                   prefix = prefix,
                   default = default,
                   keys = keys,
                   columns = columns,
                   skip = skip,
                   id = id
    )
  }

step_epi_shift <-
  function(recipe,
           ...,
           role,
           trained,
           shift,
           prefix,
           default,
           keys,
           columns,
           skip,
           id) {
    add_step(
      recipe,
      step_epi_shift_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        shift = shift,
        prefix = prefix,
        default = default,
        keys = keys,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_epi_shift_new <-
  function(terms, role, trained, shift, prefix, default, keys,
           columns, skip, id) {
    step(
      subclass = "epi_shift",
      terms = terms,
      role = role,
      trained = trained,
      shift = shift,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_epi_shift <- function(x, training, info = NULL, ...) {
  step_epi_shift_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    shift = x$shift,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_shift <- function(object, new_data, ...) {
  if (!all(object$shift == as.integer(object$shift))) {
    rlang::abort("step_epi_shift requires 'shift' argument to be integer valued.")
  }
  grid <- tidyr::expand_grid(col = object$columns, lag_val = -object$shift)
  is_lag <- object$role == "predictor"
  if (!is_lag) {
    grid <- dplyr::mutate(grid,ahead_val = -lag_val)
  }
  grid <- dplyr::mutate(grid,
      newname = glue::glue(
        paste0(
          "{object$prefix}",
          ifelse(is_lag,"{lag_val}","{ahead_val}"),
          "_{col}"
          )
        )
    )
  if (!is_lag) {
    grid <- dplyr::select(grid, -ahead_val)
  }
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
  shifted <- purrr::reduce(
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
print.step_epi_shift <-
  function(x, width = max(20, options()$width - 30), ...) {
    ## TODO add printing of the shifts
    title <- ifelse(x$role == "predictor","Lagging","Leading") %>%
      paste0(": ", abs(x$shift),",")
    recipes::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }
