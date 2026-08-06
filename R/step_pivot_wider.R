

#' Create new variables by pivotting data
#'
#' This function typically creates new predictors by sharing values across keys.
#' So in the most basic case (see examples below), the values of a signal in
#' one `geo_value` would be used as predictors in all the other locations.
#'
#' @inheritParams step_growth_rate
#' @param ... <[`tidy-select`][tidyr_tidy_select]> One or more selector
#'  functions to choose variables
#'  values to pivot. These are the `values_from` argument for [tidyr::pivot_wider()].
#'  See [recipes::selections()] for more details.
#' @param names_from A selector function to choose which column (or columns) to
#'  get the name of the output columns from. This is typically `geo_value`
#'  (the default), and possibly any additional keys in the training data.
#' @param id_cols <[`tidy-select`][tidyr_tidy_select]> A selector function
#'  providing a set of columns that uniquely identifies each observation.
#'  The typical use is for this to be `time_value` and any additional keys
#'  not selected by `names_from` (this is the default behaviour).
#' @inheritParams tidyr::pivot_wider
#'
#' @template step-return
#' @export
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   filter(geo_value %in% c("ca", "ny", "pa"), time_value > "2021-12-01")
#' r <- epi_recipe(jhu)
#'
#' r1 <- r %>% step_pivot_wider("death_rate")
#' bake(prep(r1, jhu), new_data = NULL)
#'
#' r2 <- r %>% step_pivot_wider(dplyr::ends_with("rate"))
#' bake(prep(r2, jhu), new_data = NULL)
step_pivot_wider <- function(
    recipe,
    ...,
    names_from = "geo_value",
    role = "predictor",
    id_cols = "time_value",
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
  hardhat::validate_column_names(training, user_id_cols)
  names_from <- recipes_eval_select(x$names_from, training, info)
  remaining_ids <- setdiff(
    union(user_id_cols, names_from), # keys from user
    key_colnames(training)           # all edf keys
  )
  all_id_cols <- union(user_id_cols, remaining_ids)
  step_pivot_wider_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    user_id_cols = user_id_cols,
    edf_id_cols = all_id_cols,
    id_expand = x$id_expand,
    names_from = names_from,
    values_fill = x$values_fill,
    values_fn = x$values_fn,
    values_from = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_pivot_wider <- function(object, new_data, ...) {
  id_cols <- object$edf_id_cols
  names_from <- object$names_from
  values_from <- object$values_from
  browser()
  hardhat::validate_column_names(new_data, id_cols)
  hardhat::validate_column_names(new_data, names_from)
  hardhat::validate_column_names(new_data, values_from)
  if (length(id_cols) == 0L) {
    pivotted <- tidyr::pivot_wider(
      new_data[, c(names_from, values_from)],
      id_expand = object$id_expand,
      names_from = unname(names_from),
      values_from = unname(values_from),
      values_fill = object$values_fill,
      values_fn = object$values_fn,
      names_repair = "unique"
    )
    joinby <- intersect(names(pivotted), names(new_data))
  } else {
    pivotted <- tidyr::pivot_wider(
      new_data[, c(id_cols, names_from, values_from)],
      id_cols = unname(id_cols),
      id_expand = object$id_expand,
      names_from = unname(names_from),
      values_from = unname(values_from),
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

