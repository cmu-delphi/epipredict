#' Calculate a rolling sum
#'
#' `step_epi_slide_sum()` creates a *specification* of a recipe step that will
#'   generate one or more new columns of derived data by computing a sliding
#'   sum along existing data.
#'
#'
#' @inheritParams step_epi_lag
#' @param before,after non-negative integers.
#'   How far `before` and `after` each `time_value` should
#'   the sliding window extend? Any value provided for either
#'   argument must be a single, non-`NA`, non-negative,
#'   [integer-compatible][vctrs::vec_cast] number of time steps. Endpoints of
#'   the window are inclusive. Common settings:
#'   * For trailing/right-aligned windows from `time_value - time_step(k)` to
#'   `time_value`, use `before=k, after=0`. This is the most likely use case
#'   for the purposes of forecasting.
#'   * For center-aligned windows from `time_value - time_step(k)` to
#'   `time_value + time_step(k)`, use `before=k, after=k`.
#'   * For leading/left-aligned windows from `time_value` to
#'   `time_value + time_step(k)`, use `after=k, after=0`.
#'
#'   You may also pass a [lubridate::period], like `lubridate::weeks(1)` or a
#'   character string that is coercible to a [lubridate::period], like
#'   `"2 weeks"`.
#' @template step-return
#'
#' @export
#' @examples
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value >= as.Date("2021-01-01"), geo_value %in% c("ca", "ny"))
#' rec <- epi_recipe(jhu) %>%
#'   step_epi_slide_sum(case_rate, death_rate,
#'     before = 6L
#'   )
#' bake(prep(rec, jhu), new_data = NULL)
step_epi_slide_sum <-
  function(recipe,
           ...,
           before = 0L,
           after = 0L,
           role = "predictor",
           prefix = "epi_slide_sum_",
           skip = FALSE,
           id = rand_id("epi_slide_sum")) {
    if (!is_epi_recipe(recipe)) {
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")
    }
    arg_is_scalar(before, after)
    before <- try_period(before)
    after <- try_period(after)
    arg_is_chr_scalar(role, prefix, id)
    arg_is_lgl_scalar(skip)
    add_step(
      recipe,
      step_epi_slide_sum_new(
        terms = enquos(...),
        before = before,
        after = after,
        role = role,
        trained = FALSE,
        prefix = prefix,
        keys = epi_keys(recipe),
        columns = NULL,
        skip = skip,
        id = id
      )
    )
  }


step_epi_slide_sum_new <-
  function(terms,
           before,
           after,
           role,
           trained,
           prefix,
           keys,
           columns,
           skip,
           id) {
    step(
      subclass = "epi_slide_sum",
      terms = terms,
      before = before,
      after = after,
      role = role,
      trained = trained,
      prefix = prefix,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }



#' @export
prep.step_epi_slide_sum <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)

  check_type(training[, col_names], types = c("double", "integer"))
  time_type <- attributes(training)$metadata$time_type
  before <- lubridate_period_to_integer(x$before, time_type)
  after <- lubridate_period_to_integer(x$after, time_type)
  step_epi_slide_sum_new(
    terms = x$terms,
    before = before,
    after = after,
    role = x$role,
    trained = TRUE,
    prefix = x$prefix,
    keys = x$keys,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_slide_sum <- function(object, new_data, ...) {
  recipes::check_new_data(names(object$columns), object, new_data)
  col_names <- as.vector(object$columns)
  name_prefix <- object$prefix
  new_names <- glue::glue("{name_prefix}{col_names}")
  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% new_names
  if (any(intersection)) {
    nms <- new_data_names[intersection]
    cli_abort(
      c("In `step_epi_slide_sum()` a name collision occurred. The following variable names already exist:",
        `*` = "{.var {nms}}"
      ),
      call = caller_env(),
      class = "epipredict__step__name_collision_error"
    )
  }
  renaming <- glue::glue("slide_value_{col_names}")
  names(renaming) <- new_names
  names(new_names) <- glue::glue("slide_value_{col_names}")
  new_data %>%
    group_by(across(all_of(object$keys[-1]))) %>%
    epi_slide_sum(col_names, before = object$before, after = object$after) %>%
    rename(all_of(renaming))
}


#' @export
print.step_epi_slide_sum <- function(x, width = max(20, options()$width - 30), ...) {
  print_epi_step(
    x$columns, x$terms, x$trained,
    title = "Calculating epi_slide for ",
    conjunction = "with", extra_text = x$f_name
  )
  invisible(x)
}
