#' Calculate a rolling window transformation
#'
#' `step_epi_slide()` creates a *specification* of a recipe step
#'   that will generate one or more new columns of derived data by "sliding"
#'   a computation along existing data.
#'
#'
#' @inheritParams step_epi_lag
#' @param .f A function in one of the following formats:
#'  1. An unquoted function name with no arguments, e.g., `mean`
#'  2. A character string name of a function, e.g., `"mean"`. Note that this
#'     can be difficult to examine for mistakes (so the misspelling `"maen"`
#'     won't produce an error until you try to actually fit the model)
#'  3. A base `R` lambda function, e.g., `function(x) mean(x, na.rm = TRUE)`
#'  4. A new-style base `R` lambda function, e.g., `\(x) mean(x, na.rm = TRUE)`
#'  5. A one-sided formula like `~ mean(.x, na.rm = TRUE)`.
#'
#'  Note that in cases 3 and 4, `x` can be any variable name you like (for
#'  example `\(dog) mean(dog, na.rm = TRUE)` will work). But in case 5, the
#'  argument must be named `.x`. A common, though very difficult to debug
#'  error is using something like `function(x) mean`. This will not work.
#' @param f_name a character string of at most 20 characters that describes
#'   the function. This will be combined with `prefix` and the columns in `...`
#'   to name the result using `{prefix}{f_name}_{column}`. It will be determined
#'   automatically using `clean_f_name()`.
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
#'   step_epi_slide(case_rate, death_rate,
#'     .f = ~ mean(.x, na.rm = TRUE),
#'     before = 6L
#'   )
#' bake(prep(rec, jhu), new_data = NULL)
step_epi_slide <-
  function(recipe,
           ...,
           .f,
           before = 0L,
           after = 0L,
           role = "predictor",
           prefix = "epi_slide_",
           f_name = clean_f_name(.f),
           skip = FALSE,
           id = rand_id("epi_slide")) {
    if (!is_epi_recipe(recipe)) {
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")
    }
    .f <- validate_slide_fun(.f)
    arg_is_scalar(before, after)
    before <- try_period(before)
    after <- try_period(after)
    arg_is_chr_scalar(role, prefix, id)
    arg_is_lgl_scalar(skip)

    add_step(
      recipe,
      step_epi_slide_new(
        terms = enquos(...),
        before = before,
        after = after,
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
        `*` = "{.var {nms}}"
      ),
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
          .x,
          .i = time_value,
          object$.f, .before = object$before, .after = object$after
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

#' Create short function names
#'
#' @param .f a function, character string, or lambda. For example, `mean`,
#'   `"mean"`, `~ mean(.x)` or `\(x) mean(x, na.rm = TRUE)`.
#' @param max_length integer determining how long names can be
#'
#' @return a character string of length at most `max_length` that
#'   (partially) describes the function.
#' @export
#'
#' @examples
#' clean_f_name(mean)
#' clean_f_name("mean")
#' clean_f_name(~ mean(.x, na.rm = TRUE))
#' clean_f_name(\(x) mean(x, na.rm = TRUE))
#' clean_f_name(function(x) mean(x, na.rm = TRUE, trim = 0.2357862))
clean_f_name <- function(.f, max_length = 20L) {
  if (rlang::is_formula(.f, scoped = TRUE)) {
    f_name <- rlang::f_name(.f)
  } else if (rlang::is_character(.f)) {
    f_name <- .f
  } else if (rlang::is_function(.f)) {
    f_name <- as.character(substitute(.f))
    if (length(f_name) > 1L) {
      f_name <- f_name[3]
      if (nchar(f_name) > max_length - 5L) {
        f_name <- paste0(substr(f_name, 1L, max(max_length - 8L, 5L)), "...")
      }
      f_name <- paste0("[ ]{", f_name, "}")
    }
  }
  if (nchar(f_name) > max_length) {
    f_name <- paste0(substr(f_name, 1L, max_length - 3L), "...")
  }
  f_name
}


validate_slide_fun <- function(.f) {
  if (rlang::quo(.f) %>% rlang::quo_is_missing()) {
    cli_abort("In, `step_epi_slide()`, `.f` may not be missing.")
  }
  if (rlang::is_formula(.f, scoped = TRUE)) {
    if (!is.null(rlang::f_lhs(.f))) {
      cli_abort("In, `step_epi_slide()`, `.f` must be a one-sided formula.")
    }
  } else if (rlang::is_character(.f)) {
    .f <- rlang::as_function(.f)
  } else if (!rlang::is_function(.f)) {
    cli_abort("In, `step_epi_slide()`, `.f` must be a function.")
  }
  .f
}

try_period <- function(x) {
  err <- is.na(x)
  if (!err) {
    if (is.numeric(x)) {
      err <- !rlang::is_integerish(x) || x < 0
    } else {
      x <- lubridate::as.period(x)
      err <- is.na(x)
    }
  }
  if (err) {
    cli_abort(paste(
      "The value supplied to `before` or `after` must be a non-negative integer",
      "a {.cls lubridate::period} or a character scalar that can be coerced",
      'as a {.cls lubridate::period}, e.g., `"1 week"`.'
    ), )
  }
  x
}
