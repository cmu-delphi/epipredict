#' Create a lagged predictor
#'
#' `step_epi_lag` creates a *specification* of a recipe step that
#'   will add new columns of lagged data. Lagged data will
#'   by default include NA values where the lag was induced.
#'   These can be removed with [step_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this step. See [selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#'  they be assigned? By default, the new columns created by this step from
#'  the original variables will be used as _predictors_ in a model.
#' @param trained A logical to indicate if the quantities for
#'  preprocessing have been estimated.
#' @param lag A vector of positive integers. Each specified column will be
#'  lagged for each value in the vector.
#' @param prefix A prefix for generated column names, default to "lag_".
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param default Determines what fills empty rows
#'   left by lagging (defaults to NA).
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for lagging.
#'
#' @family row operation steps
#' @export
#' @rdname step_epi_lag
#'
#' @examples
#' n <- 10
#' start <- as.Date("1999/01/01")
#' end <- as.Date("1999/01/10")
#'
#' df <- data.frame(
#'   x = runif(n),
#'   index = 1:n,
#'   day = seq(start, end, by = "day")
#' )
#'
#' library(recipes)
#' recipe(~., data = df) %>%
#'   step_lag(index, day, lag = 2:3) %>%
#'   prep(df) %>%
#'   bake(df)
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
    add_step(
      recipe,
      step_epi_lag_new(
        terms = dplyr::enquos(...),
        role = role,
        trained = trained,
        lag = lag,
        prefix = prefix,
        default = default,
        keys = keys,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_epi_lag_new <-
  function(terms, role, trained, lag, prefix, default, keys,
           columns, skip, id) {
    step(
      subclass = "epi_lag",
      terms = terms,
      role = role,
      trained = trained,
      lag = lag,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_epi_lag <- function(x, training, info = NULL, ...) {
  step_epi_lag_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    lag = x$lag,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_epi_lag <- function(object, new_data, ...) {
  if (!all(object$lag == as.integer(object$lag))) {
    rlang::abort("step_epi_lag requires 'lag' argument to be integer valued.")
  }

  grid <- tidyr::expand_grid(col = object$columns, lag_val = object$lag) %>%
    dplyr::mutate(newname = glue::glue("{object$prefix}{lag_val}_{col}"))

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

  lagged <- purrr::reduce(
    purrr::pmap(grid, epi_shift_single, x = new_data, key_cols = object$keys),
    dplyr::full_join,
    by = object$keys
  )

  dplyr::full_join(new_data, lagged, by = object$keys)
}

#' @export
print.step_epi_lag <-
  function(x, width = max(20, options()$width - 30), ...) {
    ## TODO add printing of the lags
    title <- "Lagging "
    recipes::print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }
