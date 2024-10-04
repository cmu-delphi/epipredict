

#' Create new variables by pivotting data
#'
#' @inheritParams step_growth_rate
#' @param ... One or more selector functions to choose variables
#'  values to pivot. These are the `values_from` argument for [tidyr::pivot_wider()].
#'  See [recipes::selections()] for more details.
#' @inheritParams tidyr::pivot_wider
#'
#' @template step-return
#' @export
#'
#' @examples
#' 1+1
step_pivot_wider <- function(
    recipe,
    ...,
    role = "predictor",
    names_from,
    id_cols = NULL,
    id_expand = FALSE,
    values_fill = NA,
    values_fn = NULL,
    skip = FALSE,
    id = rand_id("pivot_wider")
) {

  arg_is_chr_scalar(role, id)

  id_cols <- enquos(id_cols)
  names_from <- enquos(names_from)

  add_step(
    recipe,
    step_pivot_wider_new(
      terms = enquos(...),
      role = role,
      trained = FALSE,
      user_id_cols = id_cols,
      edf_id_cols = key_colnames(recipe),
      id_expand = id_expand,
      names_from = names_from,
      values_fill = values_fill,
      values_fn = values_fn,
      values_from = NULL,
      skip = skip,
      id = id
    )
  )
}

step_pivot_wider_new <- function(
    terms, role, trained, user_id_cols, edf_id_cols,
    id_expand, names_from, values_fill,
    values_fn, values_from, skip, id) {
  step(
    subclass = "pivot_wider",
    terms = terms,
    role = role,
    trained = trained,
    user_id_cols = user_id_cols,
    edf_id_cols = edf_id_cols,
    id_expand = id_expand,
    names_from = names_from,
    values_fill = values_fill,
    values_fn = values_fn,
    values_from = values_from,
    skip = skip,
    id = id
  )
}

#' @export
prep.step_pivot_wider <- function(x, training, info = NULL, ...) {
  user_id_cols <- recipes_eval_select(x$user_id_cols, training, info)
  all_id_cols <- union(user_id_cols, key_colnames(training))
  hardhat::validate_column_names(training, all_id_cols)
  step_pivot_wider_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    user_id_cols = user_id_cols,
    edf_id_cols = all_id_cols,
    id_expand = x$id_expand,
    names_from = recipes_eval_select(x$names_from, training, info),
    values_fill = x$values_fill,
    values_fn = x$values_fn,
    values_from = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pivot_wider <- function(object, new_data, ...) {
  hardhat::validate_column_names(new_data, object$all_id_cols)
  id_cols <- union(object$all_id_cols, key_colnames(new_data))
  object$edf_id_cols <- id_cols
  if (length(id_cols) == 0L) {
    pivotted <- tidyr::pivot_wider(
      new_data,
      id_expand = object$id_expand,
      names_from = unname(object$names_from),
      values_from = unname(object$values_from),
      values_fill = object$values_fill,
      values_fn = object$values_fn,
      names_repair = "unique"
    )
    joinby <- intersect(names(pivotted), names(new_data))
  } else {
    pivotted <- tidyr::pivot_wider(
      new_data,
      id_cols = unname(id_cols),
      id_expand = object$id_expand,
      names_from = unname(object$names_from),
      values_from = unname(object$values_from),
      values_fill = object$values_fill,
      values_fn = object$values_fn,
      names_repair = "unique"
    )
    joinby <- id_cols
  }
  if (length(joinby) > 0L) {
    new_data <- left_join(new_data, pivotted, by = joinby)
  } else if (length(joinby) == 0L && nrow(pivotted) == nrow(new_data)) {
    new_data <- bind_cols(new_data, pivotted, .name_repair = "unique")
  } else {
    cli_abort(c(
      "Unable to join variables created by `step_pivot_wider()`.",
      i = "You may want to pass in `id_cols` on step creation."
    ))
  }
  new_data
}

#' @export
print.step_pivot_wider <- function(x, width = max(20, options()$width - 30), ...) {
  print_epi_step(x$values_from, x$terms, x$trained,
                 title = "Pivotting variables",
                 conjunction = "by",
                 extra_text = x$names_from
  )
  invisible(x)
}

