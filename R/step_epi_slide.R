#' Calculate a rolling window transformation
#'
#' `step_epi_slide()` creates a *specification* of a recipe step
#'   that will generate one or more new columns of derived data by "sliding"
#'   a computation along existing data.
#'
#'
#' @inheritParams step_epi_lag
#' @inheritParams epiprocess::epi_slide
#' @param .f A function in one of the following formats:
#'  1. An unquoted function name with no arguments, e.g., `mean`
#'  2. A base `R` lambda function, e.g., `function(x) mean(x, na.rm = TRUE)`
#'  3. A new-style base `R` lambda function, e.g., `\(x) mean(x, na.rm = TRUE)`
#'  4. A one-sided formula like `~ mean(.x, na.rm = TRUE)`.
#'
#'  Note that in cases 2 and 3, `x` can be any variable name you like (for
#'  example `\(dog) mean(dog, na.rm = TRUE)` will work). But in case 4, the
#'  argument must be named `.x`. A common, though very difficult to debug
#'  error is using something like `function(x) mean`. This will not work.
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
#'   You may also pass a [lubridate::period], like `lubridate::weeks(1)`.
#' @template step-return
#'
#' @export
#' @examples
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value >= as.Date("2021-01-01"), geo_value %in% c("ca", "ny"))
#' rec <- epi_recipe(jhu) %>%
#'   step_epi_slide(case_rate, death_rate,
#'     .f = ~ mean(.x, na.rm = TRUE),
#'     before = 6L
#'   )
#' bake(prep(rec, jhu), new_data = NULL)
step_epi_slide <-
  function(recipe,
           ...,
           .f,
           before,
           after = 0L,
           role = "predictor",
           prefix = "epi_slide_",
           skip = FALSE,
           id = rand_id("epi_slide")) {
    if (!is_epi_recipe(recipe)) {
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")
    }
    if (rlang::quo(.f) %>% rlang::quo_is_missing()) {
      cli_abort("In, `step_epi_slide()`, `.f` may not be missing.")
    }
    if (rlang::is_formula(.f)) {
      if (!is.null(rlang::f_lhs(.f))) {
        cli_abort("In, `step_epi_slide()`, `.f` must be a one-sided formula.")
      }
      f_name <- rlang::f_name(.f)
    } else if (rlang::is_character(.f)) {
      f_name <- paste0(.f, "(.x)")
      .f <- rlang::as_function(.f)
    } else if (rlang::is_function(.f)) {
      f_name <- as.character(rlang::fn_body(.f))[2]
    } else {
      cli_abort("In, `step_epi_slide()`, `.f` must be a function.")
    }
    if (nchar(f_name) > 20L) f_name <- paste0(substr(f_name, 1L, 17L), "...")

    if (is.numeric(before)) {
      arg_is_nonneg_int(before)
    } else {
      checkmate::assert_class(before, "Period")
    }
    if (is.numeric(after)) {
      arg_is_nonneg_int(after)
    } else {
      checkmate::assert_class(after, "Period")
    }
    arg_is_chr(role)
    arg_is_chr_scalar(prefix, id)
    arg_is_lgl_scalar(skip)

    add_step(
      recipe,
      step_epi_slide_new(
        terms = enquos(...),
        before = enquo(before),
        after = enquo(after),
        .f = .f,
        f_name = f_name,
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


step_epi_slide_new <-
  function(terms,
           before,
           after,
           .f,
           f_name,
           role,
           trained,
           prefix,
           keys,
           columns,
           skip,
           id) {
    step(
      subclass = "epi_slide",
      terms = terms,
      before = before,
      after = after,
      .f = .f,
      f_name = f_name,
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
prep.step_epi_slide <- function(x, training, info = NULL, ...) {

  col_names <- recipes::recipes_eval_select(x$terms, data = training, info = info)

  check_type(training[, col_names], types = c("double", "integer"))

  step_epi_slide_new(
    terms = x$terms,
    before = x$before,
    after = x$after,
    .f = x$.f,
    f_name = x$f_name,
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
bake.step_epi_slide <- function(object, new_data, ...) {

  recipes::check_new_data(names(object$columns), object, new_data)
  col_names <- object$columns
  name_prefix <- paste0(object$prefix, object$f_name, "_")
  newnames <- glue::glue("{name_prefix}{col_names}")
  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% newnames
  if (any(intersection)) {
    nms <- new_data_names[intersection]
    cli_abort(
      c("Name collision occured. The following variable names already exist:",
      `*` = "{.var {nms}}"),
      call = caller_env()
    )
  }

  ok <- object$keys
  names(col_names) <- newnames
  gr <- new_data %>%
    dplyr::select(dplyr::all_of(c(ok, object$columns))) %>%
    group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(object$columns),
        ~ slider::slide_index_vec(
          .x, .i = time_value,
          object$.f, .before = !!object$before, .after = !!object$after
        )
      )
    ) %>%
    dplyr::rename(dplyr::all_of(col_names)) %>%
    dplyr::ungroup()

  dplyr::left_join(new_data, gr, by = ok)
}


#' @export
print.step_epi_slide <- function(x, width = max(20, options()$width - 30), ...) {
  print_epi_step(
    x$columns, x$terms, x$trained,
    title = "Calculating epi_slide for ",
    conjunction = "with", extra_text = x$f_name
  )
  invisible(x)
}
