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
#' @param additional_gr_args_list A list of additional arguments used by
#'   [epiprocess::growth_rate()]. All `...` arguments may be passed here along
#'   with `dup_rm` and `na_rm`.
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_.
#'
#'
#' @family row operation steps
#' @importFrom epiprocess growth_rate
#' @export
#' @examples
#' r <- epi_recipe(case_death_rate_subset) %>%
#'   step_growth_rate(case_rate, death_rate)
#' r
step_growth_rate <-
  function(
    recipe,
    ...,
    role = "predictor",
    trained = FALSE,
    horizon = 7,
    method = c("rel_change", "linear_reg", "smooth_spline", "trend_filter"),
    log_scale = FALSE,
    prefix = "gr_",
    columns = NULL,
    skip = FALSE,
    id = rand_id("growth_rate"),
    additional_gr_args_list = list()
  ) {

    if (!is_epi_recipe(recipe))
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")
    method = match.arg(method)
    arg_is_pos_int(horizon)
    arg_is_scalar(horizon)
    arg_is_chr(role)
    arg_is_chr_scalar(prefix, id)
    arg_is_lgl_scalar(trained, log_scale, skip)
    if (!is.list(additional_gr_args_list)) {
      rlang::abort(
        c("`additional_gr_args_list` must be a list.",
          i = "See `?epiprocess::growth_rate` for available options."))
    }

    if (!is.null(columns)) {
      rlang::abort(c("The `columns` argument must be `NULL.",
                     i = "Use `tidyselect` methods to choose columns to use."))
    }

    add_step(recipe,
             step_growth_rate_new(
               terms = dplyr::enquos(...),
               role = role,
               trained = trained,
               horizon = horizon,
               method = method,
               log_scale = log_scale,
               prefix = prefix,
               keys = epi_keys(recipe),
               columns = columns,
               skip = skip,
               id = id,
               additional_gr_args_list = additional_gr_args_list
             ))
  }


step_growth_rate_new <-
  function(terms,
           role,
           trained,
           horizon,
           method,
           log_scale,
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
        i = paste("The following variable names already exists: ",
             paste0(new_data_names[intersection], collapse = ", "),
             ".")
      ))
  }

  ok <- object$keys
  gr <- new_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::transmute(
      time_value = time_value,
      dplyr::across(
        dplyr::all_of(object$columns),
        ~ epiprocess::growth_rate(
          time_value, .x, method = object$method,
          h = object$horizon, log_scale = object$log_scale,
          !!!object$additional_gr_args_list
        ),
        .names = "{object$prefix}{object$horizon}_{object$method}_{.col}"
      )) %>%
    dplyr::mutate(time_value = time_value + object$horizon) # shift x0 right

  dplyr::left_join(new_data, gr, by = ok) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::ungroup()
}


#' @export
print.step_growth_rate <- function(x, width = max(20, options()$width - 30), ...) {
  print_step_shift(x$columns, x$terms, x$trained, "Calculating growth_rate for ",
                   shift = x$method)
  invisible(x)
}
