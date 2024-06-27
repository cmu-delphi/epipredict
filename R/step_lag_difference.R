#' Calculate a lagged difference
#'
#' `step_lag_difference()` creates a *specification* of a recipe step
#'   that will generate one or more new columns of derived data.
#'
#'
#' @inheritParams step_epi_lag
#' @param horizon Scalar or vector. Time period(s) over which to calculate
#'   differences.
#'
#' @template step-return
#'
#'
#'
#' @family row operation steps
#' @export
#' @examples
#' r <- epi_recipe(case_death_rate_subset) %>%
#'   step_lag_difference(case_rate, death_rate, horizon = c(7, 14)) %>%
#'   step_epi_naomit()
#' r
#'
#' r %>%
#'   recipes::prep(case_death_rate_subset) %>%
#'   recipes::bake(case_death_rate_subset)
step_lag_difference <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           horizon = 7,
           prefix = "lag_diff_",
           columns = NULL,
           skip = FALSE,
           id = rand_id("lag_diff")) {
    if (!is_epi_recipe(recipe)) {
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")
    }
    arg_is_pos_int(horizon)
    arg_is_chr(role)
    arg_is_chr_scalar(prefix, id)
    arg_is_lgl_scalar(trained, skip)

    if (!is.null(columns)) {
      rlang::abort(
        c("The `columns` argument must be `NULL.",
          i = "Use `tidyselect` methods to choose columns to use."
        )
      )
    }

    add_step(
      recipe,
      step_lag_difference_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        horizon = horizon,
        prefix = prefix,
        keys = epi_keys(recipe),
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }


step_lag_difference_new <-
  function(terms,
           role,
           trained,
           horizon,
           prefix,
           keys,
           columns,
           skip,
           id) {
    step(
      subclass = "lag_difference",
      terms = terms,
      role = role,
      trained = trained,
      horizon = horizon,
      prefix = prefix,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }



#' @export
prep.step_lag_difference <- function(x, training, info = NULL, ...) {
  step_lag_difference_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    horizon = x$horizon,
    prefix = x$prefix,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}


epi_shift_single_diff <- function(x, col, horizon, newname, key_cols) {
  x <- x %>% dplyr::select(tidyselect::all_of(c(key_cols, col)))
  y <- x %>%
    dplyr::mutate(time_value = time_value + horizon) %>%
    dplyr::rename(!!newname := {{ col }})
  x <- dplyr::left_join(x, y, by = key_cols)
  x[, newname] <- x[, col] - x[, newname]
  x %>% dplyr::select(tidyselect::all_of(c(key_cols, newname)))
}


#' @export
bake.step_lag_difference <- function(object, new_data, ...) {
  grid <- tidyr::expand_grid(col = object$columns, horizon = object$horizon) %>%
    dplyr::mutate(newname = glue::glue("{object$prefix}{horizon}_{col}"))

  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% grid$newname
  if (any(intersection)) {
    rlang::abort(
      c(paste0("Name collision occured in `", class(object)[1], "`."),
        i = paste(
          "The following variable names already exists: ",
          paste0(new_data_names[intersection], collapse = ", "),
          "."
        )
      )
    )
  }

  ok <- object$keys
  shifted <- reduce(
    pmap(grid, epi_shift_single_diff, x = new_data, key_cols = ok),
    dplyr::full_join,
    by = ok
  )

  dplyr::left_join(new_data, shifted, by = ok) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ok[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::ungroup()
}


#' @export
print.step_lag_difference <- function(x, width = max(20, options()$width - 30), ...) {
  print_epi_step(x$columns, x$terms, x$trained,
    title = "Calculating lag_difference for",
    conjunction = "by",
    extra_text = x$horizon
  )
  invisible(x)
}
