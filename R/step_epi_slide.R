#' Calculate a rolling window transformation
#'
#' `step_epi_slide()` creates a *specification* of a recipe step that will
#'   generate one or more new columns of derived data by "sliding" a computation
#'   along existing data. This is a wrapper around `epiprocess::epi_slide()`
#'   to allow its use within an `epi_recipe()`.
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
#'  error is using something like `function(x) mean`. This will not work
#'  because it returns the function mean, rather than `mean(x)`
#' @param .window_size the size of the sliding window, required. Usually a
#'  non-negative integer will suffice (e.g. for data indexed by date, but more
#'  restrictive in other time_type cases (see [epiprocess::epi_slide()] for
#'  details). For example, set to 7 for a 7-day window.
#' @param .align a character string indicating how the window should be aligned.
#'  By default, this is "right", meaning the slide_window will be anchored with
#'  its right end point on the reference date. (see [epiprocess::epi_slide()]
#'  for details).
#' @param f_name a character string of at most 20 characters that describes the
#'   function. This will be combined with `prefix` and the columns in `...` to
#'   name the result using `{prefix}{f_name}_{column}`. By default it will be
#'   determined automatically using `clean_f_name()`.
#'
#' @template step-return
#'
#' @export
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value >= as.Date("2021-01-01"), geo_value %in% c("ca", "ny"))
#' rec <- epi_recipe(jhu) %>%
#'   step_epi_slide(case_rate, death_rate,
#'     .f = \(x) mean(x, na.rm = TRUE),
#'     .window_size = 7L
#'   )
#' bake(prep(rec, jhu), new_data = NULL)
step_epi_slide <- function(recipe,
                           ...,
                           .f,
                           .window_size = NULL,
                           .align = c("right", "center", "left"),
                           role = "predictor",
                           prefix = "epi_slide_",
                           f_name = clean_f_name(.f),
                           skip = FALSE,
                           id = rand_id("epi_slide")) {
  if (!is_epi_recipe(recipe)) {
    cli_abort("This recipe step can only operate on an {.cls epi_recipe}.")
  }
  .f <- validate_slide_fun(.f)
  if (is.null(.window_size)) {
    cli_abort("step_epi_slide: `.window_size` must be specified.")
  }
  epiprocess:::validate_slide_window_arg(.window_size, attributes(recipe$template)$metadata$time_type)
  .align <- rlang::arg_match(.align)
  arg_is_chr_scalar(role, prefix, id)
  arg_is_lgl_scalar(skip)

  recipes::add_step(
    recipe,
    step_epi_slide_new(
      terms = enquos(...),
      .window_size = .window_size,
      .align = .align,
      .f = .f,
      f_name = f_name,
      role = role,
      trained = FALSE,
      prefix = prefix,
      keys = key_colnames(recipe),
      columns = NULL,
      skip = skip,
      id = id
    )
  )
}


step_epi_slide_new <-
  function(terms,
           .window_size,
           .align,
           .f,
           f_name,
           role,
           trained,
           prefix,
           keys,
           columns,
           skip,
           id) {
    recipes::step(
      subclass = "epi_slide",
      terms = terms,
      .window_size = .window_size,
      .align = .align,
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

  recipes::check_type(training[, col_names], types = c("double", "integer"))

  step_epi_slide_new(
    terms = x$terms,
    .window_size = x$.window_size,
    .align = x$.align,
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
      c("In `step_epi_slide()` a name collision occurred. The following variable name{?s} already exist{?/s}:",
        `*` = "{.var {nms}}"
      ),
      call = caller_env(),
      class = "epipredict__step__name_collision_error"
    )
  }
  # TODO: Uncomment this whenever we make the optimized versions available.
  # if (any(vapply(c(mean, sum), \(x) identical(x, object$.f), logical(1L)))) {
  #   cli_warn(
  #     c(
  #       "There is an optimized version of both mean and sum. See `step_epi_slide_mean`, `step_epi_slide_sum`,
  #      or `step_epi_slide_opt`."
  #     ),
  #     class = "epipredict__step_epi_slide__optimized_version"
  #   )
  # }
  epi_slide_wrapper(
    new_data,
    object$.window_size,
    object$.align,
    object$columns,
    c(object$.f),
    object$f_name,
    kill_time_value(object$keys),
    object$prefix
  )
}


#' Wrapper to handle epi_slide particulars
#'
#' @description
#' This should simplify somewhat in the future when we can run `epi_slide` on
#'   columns. Surprisingly, lapply is several orders of magnitude faster than
#'   using roughly equivalent tidy select style.
#'
#' @param fns vector of functions, even if it's length 1.
#' @param group_keys the keys to group by. likely `epi_keys` (without `time_value`)
#'
#' @importFrom tidyr crossing
#' @importFrom dplyr bind_cols group_by ungroup
#' @importFrom epiprocess epi_slide
#' @keywords internal
epi_slide_wrapper <- function(new_data, .window_size, .align, columns, fns, fn_names, group_keys, name_prefix) {
  cols_fns <- tidyr::crossing(col_name = columns, fn_name = fn_names, fn = fns)
  # Iterate over the rows of cols_fns. For each row number, we will output a
  # transformed column. The first result returns all the original columns along
  # with the new column. The rest just return the new column.
  seq_len(nrow(cols_fns)) %>%
    lapply(function(comp_i) {
      col_name <- cols_fns[[comp_i, "col_name"]]
      fn_name <- cols_fns[[comp_i, "fn_name"]]
      fn <- cols_fns[[comp_i, "fn"]][[1L]]
      result_name <- paste(name_prefix, fn_name, col_name, sep = "_")
      result <- new_data %>%
        group_by(across(all_of(group_keys))) %>%
        epi_slide(
          .window_size = .window_size,
          .align = .align,
          .new_col_name = result_name,
          .f = function(slice, geo_key, ref_time_value) {
            fn(slice[[col_name]])
          }
        ) %>%
        ungroup()

      if (comp_i == 1L) {
        result
      } else {
        result[result_name]
      }
    }) %>%
    bind_cols()
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
    cli_abort("In, `step_epi_slide()`, `.f` cannot be a formula.")
  } else if (rlang::is_character(.f)) {
    .f <- rlang::as_function(.f)
  } else if (!rlang::is_function(.f)) {
    cli_abort("In, `step_epi_slide()`, `.f` must be a function.")
  }
  .f
}
