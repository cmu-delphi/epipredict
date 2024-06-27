#' Calculate a growth rate
#'
#' `step_growth_rate()` creates a *specification* of a recipe step
#'   that will generate one or more new columns of derived data.
#'
#'
#' @inheritParams step_epi_lag
#' @param horizon Bandwidth for the sliding window, when `method` is
#'   "rel_change" or "linear_reg". See [epiprocess::growth_rate()] for more
#'   details.
#' @param method Either "rel_change", "linear_reg", "smooth_spline", or
#'   "trend_filter", indicating the method to use for the growth rate
#'   calculation. The first two are local methods: they are run in a sliding
#'   fashion over the sequence (in order to estimate derivatives and hence
#'   growth rates); the latter two are global methods: they are run once over
#'   the entire sequence. See [epiprocess::growth_rate()] for more
#'   details.
#' @param log_scale Should growth rates be estimated using the parameterization
#'   on the log scale? See details for an explanation. Default is `FALSE`.
#' @param replace_Inf Sometimes, the growth rate calculation can result in
#'   infinite values (if the denominator is zero, for example). In this case,
#'   most prediction methods will fail. This argument specifies potential
#'   replacement values. The default (`NA`) will likely result in these rows
#'   being removed from the data. Alternatively, you could specify arbitrary
#'   large values, or perhaps zero. Setting this argument to `NULL` will result
#'   in no replacement.
#' @param additional_gr_args_list A list of additional arguments used by
#'   [epiprocess::growth_rate()]. All `...` arguments may be passed here along
#'   with `dup_rm` and `na_rm`.
#' @template step-return
#'
#'
#' @family row operation steps
#' @importFrom epiprocess growth_rate
#' @export
#' @examples
#' r <- epi_recipe(case_death_rate_subset) %>%
#'   step_growth_rate(case_rate, death_rate)
#' r
#'
#' r %>%
#'   prep(case_death_rate_subset) %>%
#'   bake(case_death_rate_subset)
step_growth_rate <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           horizon = 7,
           method = c("rel_change", "linear_reg", "smooth_spline", "trend_filter"),
           log_scale = FALSE,
           replace_Inf = NA,
           prefix = "gr_",
           columns = NULL,
           skip = FALSE,
           id = rand_id("growth_rate"),
           additional_gr_args_list = list()) {
    if (!is_epi_recipe(recipe)) {
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")
    }
    method <- match.arg(method)
    arg_is_pos_int(horizon)
    arg_is_scalar(horizon)
    if (!is.null(replace_Inf)) {
      if (length(replace_Inf) != 1L) rlang::abort("replace_Inf must be a scalar.")
      if (!is.na(replace_Inf)) arg_is_numeric(replace_Inf)
    }
    arg_is_chr(role)
    arg_is_chr_scalar(prefix, id)
    arg_is_lgl_scalar(trained, log_scale, skip)


    if (!is.list(additional_gr_args_list)) {
      rlang::abort(
        c("`additional_gr_args_list` must be a list.",
          i = "See `?epiprocess::growth_rate` for available options."
        )
      )
    }

    if (!is.null(columns)) {
      rlang::abort(c("The `columns` argument must be `NULL.",
        i = "Use `tidyselect` methods to choose columns to use."
      ))
    }

    add_step(
      recipe,
      step_growth_rate_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        horizon = horizon,
        method = method,
        log_scale = log_scale,
        replace_Inf = replace_Inf,
        prefix = prefix,
        keys = epi_keys(recipe),
        columns = columns,
        skip = skip,
        id = id,
        additional_gr_args_list = additional_gr_args_list
      )
    )
  }


step_growth_rate_new <-
  function(terms,
           role,
           trained,
           horizon,
           method,
           log_scale,
           replace_Inf,
           prefix,
           keys,
           columns,
           skip,
           id,
           additional_gr_args_list) {
    step(
      subclass = "growth_rate",
      terms = terms,
      role = role,
      trained = trained,
      horizon = horizon,
      method = method,
      log_scale = log_scale,
      replace_Inf = replace_Inf,
      prefix = prefix,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id,
      additional_gr_args_list = additional_gr_args_list
    )
  }



#' @export
prep.step_growth_rate <- function(x, training, info = NULL, ...) {
  step_growth_rate_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    horizon = x$horizon,
    method = x$method,
    log_scale = x$log_scale,
    replace_Inf = x$replace_Inf,
    prefix = x$prefix,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id,
    additional_gr_args_list = x$additional_gr_args_list
  )
}



#' @export
bake.step_growth_rate <- function(object, new_data, ...) {
  newnames <- glue::glue(
    "{object$prefix}{object$horizon}_{object$method}_{object$columns}"
  )

  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% newnames
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
  gr <- new_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::transmute(
      time_value = time_value,
      dplyr::across(
        dplyr::all_of(object$columns),
        ~ epiprocess::growth_rate(
          time_value, .x,
          method = object$method,
          h = object$horizon, log_scale = object$log_scale,
          !!!object$additional_gr_args_list
        ),
        .names = "{object$prefix}{object$horizon}_{object$method}_{.col}"
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(time_value = time_value + object$horizon) # shift x0 right

  if (!is.null(object$replace_Inf)) {
    gr <- gr %>%
      dplyr::mutate(
        dplyr::across(
          !dplyr::all_of(ok),
          ~ vec_replace_inf(.x, object$replace_Inf)
        )
      )
  }

  dplyr::left_join(new_data, gr, by = ok) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::ungroup()
}


#' @export
print.step_growth_rate <- function(x, width = max(20, options()$width - 30), ...) {
  print_epi_step(
    x$columns, x$terms, x$trained,
    title = "Calculating growth_rate for ",
    conjunction = "by", extra_text = x$method
  )
  invisible(x)
}

vec_replace_inf <- function(x, replacement) {
  stopifnot(length(replacement) == 1L)
  infs <- is.infinite(x)
  if (any(is.infinite(x))) x[infs] <- replacement
  x
}
