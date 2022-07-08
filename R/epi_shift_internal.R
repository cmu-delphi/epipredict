#' Create a shifted predictor
#'
#' `step_epi_lag` and `step_epi_ahead` create a *specification* of a recipe step
#'   that will add new columns of shifted data. The former will created a lag
#'   column, while the latter will create a lead column. Shifted data will
#'   by default include NA values where the shift was induced.
#'   These can be properly removed with [step_epi_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned?
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param lag,ahead A vector of integers. Each specified column will
#'  be the lag or lead for each value in the vector. Lag integers must be
#'  nonnegative, while ahead integers must be positive.
#' @param prefix A prefix to indicate what type of variable this is
#' @param default Determines what fills empty rows
#'   left by leading/lagging (defaults to NA).
#' @param keys A character vector of the keys in an epi_df
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A unique identifier for the step
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for shifting.
#'
#' The `prefix` and `id` arguments are unchangeable to ensure that the code runs
#'  properly and to avoid inconsistency with naming. For `step_epi_ahead`, they
#'  are always set to `"ahead_"` and `"epi_ahead"` respectively, while for
#'  `step_epi_lag`, they are set to `"lag_"` and `"epi_lag`, respectively.
#'
#' @family row operation steps
#' @rdname step_epi_shift
#' @export
#' @examples
#' r <- epi_recipe(case_death_rate_subset) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(death_rate, lag = c(0,7,14))
step_epi_lag <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           lag = 1,
           prefix = "lag_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_lag")) {
    stopifnot("Lag values must be nonnegative integers" =
                all(lag>=0 & lag == as.integer(lag)))

    step_epi_shift(recipe,
                   ...,
                   role = role,
                   trained = trained,
                   shift = lag,
                   prefix = prefix,
                   default = default,
                   keys = keys,
                   columns = columns,
                   skip = skip,
                   id = id,
                   intended_direction = "epi_lag"
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
           role = "outcome",
           trained = FALSE,
           ahead = 1,
           prefix = "ahead_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_ahead")) {

    stopifnot("Ahead values must be nonnegative integers" =
                all(ahead>=0 & ahead == as.integer(ahead)))

    step_epi_shift(recipe,
                   ...,
                   role = role,
                   trained = trained,
                   shift = -ahead,
                   prefix = prefix,
                   default = default,
                   keys = keys,
                   columns = columns,
                   skip = skip,
                   id = id,
                   intended_direction = "epi_ahead"
    )
  }

step_epi_shift <-
  function(recipe,
           ...,
           role,
           trained = FALSE,
           shift = 0,
           prefix = "shift_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_shift"),
           intended_direction = NULL) {
    add_step(
      recipe,
      step_epi_shift_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        shift = shift,
        prefix = prefix,
        default = default,
        keys = keys,
        columns = columns,
        skip = skip,
        id = id,
        intended_direction = intended_direction
      )
    )
  }

step_epi_shift_new <-
  function(terms, role, trained, shift, prefix, default, keys,
           columns, skip, id, intended_direction) {
    step(
      subclass = c(intended_direction, "epi_shift"),
      terms = terms,
      role = role,
      trained = trained,
      shift = shift,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_epi_shift <- function(x, training, info = NULL, ...) {
  step_epi_shift_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    shift = x$shift,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id,
    intended_direction = x$intended_direction
  )
}

#' @export
bake.step_epi_shift <- function(object, new_data, ...) {
  grid <- tidyr::expand_grid(col = object$columns, shift_val = object$shift) %>%
    dplyr::mutate(newname = glue::glue(
                          paste0("{object$prefix}","{abs(shift_val)}","_{col}")
                        )
  )
  ## ensure no name clashes
  new_data_names <- colnames(new_data)
  intersection <- new_data_names %in% grid$newname
  if (any(intersection)) {
    rlang::abort(
      paste0("Name collision occured in `", class(object)[1],
             "`. The following variable names already exists: ",
             paste0(new_data_names[intersection], collapse = ", "),
             "."))
  }
  ok <- object$keys
  shifted <- purrr::reduce(
    purrr::pmap(grid, epi_shift_single, x = new_data, key_cols = ok),
    dplyr::full_join,
    by = ok
  )

  dplyr::full_join(new_data, shifted, by = ok) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ok[-1]))) %>%
    dplyr::arrange(time_value) %>%
    dplyr::ungroup()

}

#' @export
print.step_epi_lag <- function(x, ...) {
  NextMethod(title = "Lagging ", shift = abs(x$shift))
}

#' @export
print.step_epi_ahead <- function(x, ...) {
  NextMethod(title = "Leading ", shift = abs(x$shift))
}

#' @export
print.step_epi_shift <- function(x,
                                 width = max(20, options()$width - 30),
                                 title = "Shifting ",
                                 shift = x$shift,
                                 ...) {
  print_step_shift(x$columns, x$terms, x$trained, title, width, shift = shift)
  invisible(x)
}


print_step_shift <- function(
    tr_obj = NULL, untr_obj = NULL, trained = FALSE, title = NULL,
    width = max(20, options()$width - 30), case_weights = NULL, shift = NULL) {

  cat(title)
  if (trained) txt <- recipes::format_ch_vec(tr_obj, width = width)
  else txt <- recipes::format_selectors(untr_obj, width = width)
  if (length(txt) == 0L) txt <- "<none>"
  cat(txt)
  if (trained) {
    if (is.null(case_weights)) cat(" [trained]")
    else {
      case_weights_ind <- ifelse(case_weights, "weighted",
                                 "ignored weights")
      trained_txt <- paste(case_weights_ind, "trained",
                           sep = ", ")
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
