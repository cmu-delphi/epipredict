#' Create a shifted predictor
#'
#' `step_epi_lag` and `step_epi_ahead` create a *specification* of a recipe step
#'   that will add new columns of shifted data. The `step_epi_lag` will create
#'   a lagged `predictor` column, while `step_epi_ahead` will create a leading
#'   `outcome` column. Shifted data will by default include NA values where the
#'   shift was induced.  These can be properly removed with [step_epi_naomit()],
#'   or you may specify an alternative value with the `default` argument.
#'
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [recipes::selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? `lag` is default a predictor while `ahead` is an outcome.
#' @param lag,ahead A vector of integers. Each specified column will
#'  be the lag or lead for each value in the vector. Lag integers must be
#'  nonnegative, while ahead integers must be positive.
#' @param prefix A character string that will be prefixed to the new column.
#' @param default Determines what fills empty rows
#'   left by leading/lagging (defaults to NA).
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A unique identifier for the step
#' @template step-return
#'
#' @details
#'
#'  Our `lag/ahead` functions respect the `geo_value` and `other_keys` of the
#'   `epi_df`, and allow for discontiguous `time_value`s. Both of these features
#'   are noticably lacking from `recipe::step_lag()`.
#'  Our `lag/ahead` functions also appropriately adjust the amount of data to
#'   avoid accidentally dropping recent predictors from the test data.
#'
#' The `prefix` and `id` arguments are unchangeable to ensure that the code runs
#' properly and to avoid inconsistency with naming. For `step_epi_ahead`, they
#' are always set to `"ahead_"` and `"epi_ahead"` respectively, while for
#' `step_epi_lag`, they are set to `"lag_"` and `"epi_lag`, respectively.
#'
#' @family row operation steps
#' @rdname step_epi_shift
#' @export
#' @examples
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14))
#' r
step_epi_lag <-
  function(recipe,
           ...,
           lag,
           role = "predictor",
           prefix = "lag_",
           default = NA,
           skip = FALSE,
           id = rand_id("epi_lag")) {
    if (!is_epi_recipe(recipe)) {
      cli_abort("This step can only operate on an `epi_recipe`.")
    }

    if (missing(lag)) {
      cli_abort(c(
        "The `lag` argument must not be empty.",
        i = "Did you perhaps pass an integer in `...` accidentally?"
      ))
    }
    arg_is_nonneg_int(lag)
    arg_is_chr_scalar(prefix, id, role)
    if (role == "outcome" && length(lag) > 1L) {
      cli_abort("Only one {.val outcome} may be created with this step.")
    }

    recipes::add_step(
      recipe,
      step_epi_lag_new(
        terms = enquos(...),
        role = role,
        trained = FALSE,
        lag = as.integer(lag),
        prefix = prefix,
        default = default,
        keys = key_colnames(recipe),
        columns = NULL,
        shift_grid = NULL,
        latency_adjusted = FALSE,
        skip = skip,
        id = id
      )
    )
  }

#' Create a shifted predictor
#'
#' @family row operation steps
#' @rdname step_epi_shift
#' @export
step_epi_ahead <-
  function(recipe,
           ...,
           ahead,
           role = "outcome",
           prefix = "ahead_",
           default = NA,
           skip = FALSE,
           id = rand_id("epi_ahead")) {
    if (!is_epi_recipe(recipe)) {
      cli_abort("This step can only operate on an `epi_recipe`.")
    }

    if (missing(ahead)) {
      cli_abort(c(
        "The `ahead` argument must not be empty.",
        i = "Did you perhaps pass an integer in `...` accidentally?"
      ))
    }
    arg_is_chr_scalar(prefix, id, role)

    recipes::add_step(
      recipe,
      step_epi_ahead_new(
        terms = enquos(...),
        role = role,
        trained = FALSE,
        ahead = as.integer(ahead),
        prefix = prefix,
        default = default,
        keys = key_colnames(recipe),
        columns = NULL,
        shift_grid = NULL,
        latency_adjusted = FALSE,
        skip = skip,
        id = id
      )
    )
  }


step_epi_lag_new <-
  function(terms, role, trained, lag, prefix, default, keys,
           columns, shift_grid, latency_adjusted, skip, id) {
    recipes::step(
      subclass = "epi_lag",
      terms = terms,
      role = role,
      trained = trained,
      lag = lag,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      shift_grid = shift_grid,
      latency_adjusted = latency_adjusted,
      skip = skip,
      id = id
    )
  }

step_epi_ahead_new <-
  function(terms, role, trained, ahead, prefix, default, keys,
           columns, shift_grid, latency_adjusted, skip, id) {
    recipes::step(
      subclass = "epi_ahead",
      terms = terms,
      role = role,
      trained = trained,
      ahead = ahead,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      shift_grid = shift_grid,
      latency_adjusted = latency_adjusted,
      skip = skip,
      id = id
    )
  }



#' @export
prep.step_epi_lag <- function(x, training, info = NULL, ...) {
  columns <- recipes::recipes_eval_select(x$terms, training, info)
  if (!x$latency_adjusted) {
    tmp <- create_shift_grid(
      x$prefix,
      x$lag,
      get_sign(x),
      columns,
      attributes(training)$metadata$latency_table,
      attributes(training)$metadata$latency_sign
    )
    shift_grid <- tmp[[1]]
    latency_adjusted <- tmp[[2]]
  } else {
    shift_grid <- x$shift_grid
  }

  step_epi_lag_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lag = x$lag,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = columns,
    shift_grid = shift_grid,
    latency_adjusted = latency_adjusted,
    skip = x$skip,
    id = x$id
  )
}

#' @export
prep.step_epi_ahead <- function(x, training, info = NULL, ...) {
  columns <- recipes::recipes_eval_select(x$terms, training, info)
  if (!x$latency_adjusted) {
    tmp <- create_shift_grid(
      x$prefix,
      x$ahead,
      get_sign(x),
      columns,
      attributes(training)$metadata$latency_table,
      attributes(training)$metadata$latency_sign
    )
    shift_grid <- tmp[[1]]
    latency_adjusted <- tmp[[2]]
  } else {
    shift_grid <- x$shift_grid
  }

  step_epi_ahead_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    ahead = x$ahead,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = columns,
    shift_grid = shift_grid,
    latency_adjusted = latency_adjusted,
    skip = x$skip,
    id = x$id
  )
}



#' @export
bake.step_epi_lag <- function(object, new_data, ...) {
  add_shifted_columns(new_data, object)
}

#' @export
bake.step_epi_ahead <- function(object, new_data, ...) {
  add_shifted_columns(new_data, object)
}

#' @export
print.step_epi_lag <- function(x, width = max(20, options()$width - 30), ...) {
  if (x$latency_adjusted && x$trained) {
    lag <- x$shift_grid$shift_val
    lag <- c(lag, "(lat adj)")
  } else {
    lag <- x$lag
  }
  print_epi_step(x$columns, x$terms, x$trained, "Lagging",
    conjunction = "by",
    extra_text = lag
  )
  invisible(x)
}


#' @export
print.step_epi_ahead <- function(x, width = max(20, options()$width - 30), ...) {
  if (x$latency_adjusted && x$trained) {
    ahead <- x$shift_grid$shift_val
    ahead <- c(ahead, "(lat adj)")
  } else {
    ahead <- x$ahead
  }
  print_epi_step(x$columns, x$terms, x$trained, "Leading",
    conjunction = "by",
    extra_text = ahead
  )
  invisible(x)
}


print_step_shift <- function(
    tr_obj = NULL, untr_obj = NULL, trained = FALSE, title = NULL,
    width = max(20, options()$width - 30), case_weights = NULL, shift = NULL) {
  cat(title)
  if (trained) {
    txt <- recipes::format_ch_vec(tr_obj, width = width)
  } else {
    txt <- recipes::format_selectors(untr_obj, width = width)
  }
  if (length(txt) == 0L) txt <- "<none>"
  cat(txt)
  if (trained) {
    if (is.null(case_weights)) {
      cat(" [trained]")
    } else {
      case_weights_ind <- ifelse(case_weights, "weighted",
        "ignored weights"
      )
      trained_txt <- paste(case_weights_ind, "trained",
        sep = ", "
      )
      trained_txt <- paste0(" [", trained_txt, "]")
      cat(trained_txt)
    }
  }
  cat(" by ")
  txt <- recipes::format_ch_vec(shift)
  cat(txt)
  cat("\n")
  invisible(NULL)
}
