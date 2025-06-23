#' Calculate a lagged difference
#'
#' `step_lag_difference()` creates a *specification* of a recipe step that will
#'   generate one or more new columns of derived data. For each column in the
#'   specification, `step_lag_difference()` will calculate the difference
#'   between the values at a distance of `horizon`. For example, with
#'   `horizon=1`, this would simply be the difference between adjacent days.
#'
#' Much like `step_epi_lag()` this step works with the actual time values (so if
#' there are gaps it will fill with `NA` values), and respects the grouping
#' inherent in the `epi_df()` as specified by `geo_value` and `other_keys`.
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
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_lag_difference(case_rate, death_rate, horizon = c(7, 14)) %>%
#'   step_epi_naomit()
#' r
#'
#' r %>%
#'   prep(covid_case_death_rates) %>%
#'   bake(new_data = NULL)
step_lag_difference <-
  function(recipe,
           ...,
           role = "predictor",
           horizon = 7,
           prefix = "lag_diff_",
           skip = FALSE,
           id = rand_id("lag_diff")) {
    if (!is_epi_recipe(recipe)) {
      cli_abort("This recipe step can only operate on an {.cls epi_recipe}.")
    }
    arg_is_pos_int(horizon)
    arg_is_chr(role)
    arg_is_chr_scalar(prefix, id)
    arg_is_lgl_scalar(skip)


    recipes::add_step(
      recipe,
      step_lag_difference_new(
        terms = enquos(...),
        role = role,
        trained = FALSE,
        horizon = horizon,
        prefix = prefix,
        keys = key_colnames(recipe),
        columns = NULL,
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
    recipes::step(
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
    columns = recipes::recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}


epi_shift_single_diff <- function(x, col, horizon, newname, key_cols) {
  x <- x %>% select(all_of(c(key_cols, col)))
  y <- x %>%
    mutate(time_value = time_value + horizon) %>%
    rename(!!newname := {{ col }})
  x <- left_join(x, y, by = key_cols)
  x[, newname] <- x[, col] - x[, newname]
  x %>% select(all_of(c(key_cols, newname)))
}


#' @export
bake.step_lag_difference <- function(object, new_data, ...) {
  grid <- tidyr::expand_grid(col = object$columns, horizon = object$horizon) %>%
    mutate(newname = glue::glue("{object$prefix}{horizon}_{col}"))

  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% grid$newname
  if (any(intersection)) {
    nms <- new_data_names[intersection]
    cli_abort(
      c("In `step_lag_difference()` a name collision occurred. The following variable name{?s} already exist{?/s}:",
        `*` = "{.var {nms}}"
      ),
      call = caller_env(),
      class = "epipredict__step__name_collision_error"
    )
  }

  ok <- object$keys
  shifted <- reduce(
    pmap(grid, epi_shift_single_diff, x = new_data, key_cols = ok),
    full_join,
    by = ok
  )

  left_join(new_data, shifted, by = ok) %>%
    group_by(across(all_of(kill_time_value(ok)))) %>%
    arrange(time_value) %>%
    ungroup()
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
