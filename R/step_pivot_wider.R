step_pivot_wider <- function(
    recipe,
    ...,
    role = "predictor",
    id_cols = NULL,
    id_expand = FALSE,
    names_from = NULL,
    values_fill = NA,
    values_fn = NULL,
    unused_fn = NULL,
    skip = FALSE,
    id = rand_id("pivot_wider")
) {

  arg_is_chr_scalar(role, id)
  add_step(
    recipe,
    step_pivot_wider_new(
      terms = enquos(...),
      role = role,
      trained = FALSE,
      id_cols = id_cols,
      id_expand = id_expand,
      names_from = names_from %||% kill_time_value(key_colnames(recipe)),
      values_fill = values_fill,
      values_fn = values_fn,
      columns = NULL,
      skip = skip,
      id = id
    )
  )
}

step_pivot_wider_new <- function(
    terms, role, trained, id_cols, id_expand, names_from, values_fill,
    values_fn, columns, skip, id) {
  step(
    subclass = "pivot_wider",
    terms = terms,
    role = role,
    trained = trained,
    id_cols = id_cols,
    id_expand = id_expand,
    names_from = names_from,
    values_fill = values_fill,
    values_fn = values_fn,
    columns = columns,
    skip = skip,
    id = id
  )
}

prep.step_pivot_wider <- function(x, training, info = NULL, ...) {
  step_pivot_wider_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    id_cols = x$id_cols,
    id_expand = x$id_expand,
    names_from = x$names_from,
    values_fill = x$values_fill,
    values_fn = x$values_fn,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

bake.step_pivot_wider <- function(object, new_data, ...) {
  pivotted <- tidyr::pivot_wider(
    new_data, id_cols = object$id_cols, id_expand = object$id_expand,
    names_from = object$names_from, values_from = all_of(object$columns),
    values_fill = object$values_fill, values_fn = object$values_fn
  )
  joinby <- union(key_colnames(new_data), object$id_cols))
  new_data <- left_join(new_data, pivotted, by)

}
