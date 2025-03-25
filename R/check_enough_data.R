#' Check the dataset contains enough data points.
#'
#' `check_enough_data` creates a *specification* of a recipe
#'  operation that will check if variables contain enough data.
#'
#' @param recipe A recipe object. The check will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables for this check.
#'  See [selections()] for more details. You will usually want to use
#'  [recipes::all_predictors()] and/or [recipes::all_outcomes()] here.
#' @param n The minimum number of data points required for training. If this is
#'   NULL, the total number of predictors will be used.
#' @param epi_keys A character vector of column names on which to group the data
#'   and check threshold within each group. Useful if your forecaster trains
#'   per group (for example, per geo_value).
#' @param drop_na A logical for whether to count NA values as valid rows.
#' @param role Not used by this check since no new variables are
#'  created.
#' @param trained A logical for whether the selectors in `...`
#' have been resolved by [prep()].
#' @param columns An internal argument that tracks which columns are evaluated
#'   for this check. Should not be used by the user.
#' @param id A character string that is unique to this check to identify it.
#' @param skip A logical. If `TRUE`, only training data is checked, while if
#'   `FALSE`, both training and predicting data is checked. Technically, this
#'   answers the question "should the check be skipped when the recipe is baked
#'   by [bake()]?" While all operations are baked when [prep()] is run, some
#'   operations may not be able to be conducted on new data (e.g. processing the
#'   outcome variable(s)).  Care should be taken when using `skip = TRUE` as it
#'   may affect the computations for subsequent operations.
#' @family checks
#' @export
#' @details This check will break the `prep` and/or bake function if any of the
#'   checked columns have not enough non-NA values. If the check passes, nothing
#'   is changed in the data. It is best used after every other step.
#'
#'   For checking training data, it is best to set `...` to be
#'   `all_predictors(), all_outcomes()`, while for checking prediction data, it
#'   is best to set `...` to be `all_predictors()` only, with `n = 1`.
#'
#'  # tidy() results
#'
#'  When you [`tidy()`][tidy.recipe()] this check, a tibble with column
#'  `terms` (the selectors or variables selected) is returned.
#'
check_enough_data <-
  function(recipe,
           ...,
           n = NULL,
           epi_keys = NULL,
           drop_na = TRUE,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = TRUE,
           id = rand_id("enough_data")) {
    recipes::add_check(
      recipe,
      check_enough_data_new(
        n = n,
        epi_keys = epi_keys,
        drop_na = drop_na,
        terms = enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

check_enough_data_new <-
  function(n, epi_keys, drop_na, terms, role, trained, columns, skip, id) {
    recipes::check(
      subclass = "enough_data",
      prefix = "check_",
      n = n,
      epi_keys = epi_keys,
      drop_na = drop_na,
      terms = terms,
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.check_enough_data <- function(x, training, info = NULL, ...) {
  col_names <- recipes::recipes_eval_select(x$terms, training, info)
  if (is.null(x$n)) {
    x$n <- length(col_names)
  }

  check_enough_data_core(training, x, col_names, "train")


  check_enough_data_new(
    n = x$n,
    epi_keys = x$epi_keys,
    drop_na = x$drop_na,
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.check_enough_data <- function(object, new_data, ...) {
  col_names <- object$columns
  check_enough_data_core(new_data, object, col_names, "predict")
  new_data
}

#' @export
print.check_enough_data <- function(x, width = max(20, options()$width - 30), ...) {
  title <- paste0("Check enough data (n = ", x$n, ") for ")
  recipes::print_step(x$columns, x$terms, x$trained, title, width)
  invisible(x)
}

#' @export
tidy.check_enough_data <- function(x, ...) {
  if (recipes::is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    res <- tibble(terms = recipes::sel2char(x$terms))
  }
  res$id <- x$id
  res$n <- x$n
  res$epi_keys <- x$epi_keys
  res$drop_na <- x$drop_na
  res
}

check_enough_data_core <- function(epi_df, step_obj, col_names, train_or_predict) {
  epi_df <- epi_df %>%
    group_by(across(all_of(.env$step_obj$epi_keys)))
  if (step_obj$drop_na) {
    any_missing_data <- epi_df %>%
      mutate(any_are_na = rowSums(across(any_of(.env$col_names), ~ is.na(.x))) > 0) %>%
      # count the number of rows where they're all not na
      summarise(sum(any_are_na == 0) < .env$step_obj$n, .groups = "drop")
    any_missing_data <- any_missing_data %>%
      summarize(across(all_of(setdiff(names(any_missing_data), step_obj$epi_keys)), any)) %>%
      any()

    # figuring out which individual columns (if any) are to blame for this darth
    # of data
    cols_not_enough_data <- epi_df %>%
      summarise(
        across(
          all_of(.env$col_names),
          ~ sum(!is.na(.x)) < .env$step_obj$n
        ),
        .groups = "drop"
      ) %>%
      summarise(across(all_of(.env$col_names), any), .groups = "drop") %>%
      unlist() %>%
      names(.)[.]

    if (length(cols_not_enough_data) == 0) {
      cols_not_enough_data <-
        glue::glue("no single column, but the combination of {paste0(col_names, collapse = ', ')}")
    }
  } else {
    # if we're not dropping na values, just count
    cols_not_enough_data <- epi_df %>%
      summarise(
        across(
          all_of(.env$col_names),
          ~ dplyr::n() < .env$step_obj$n
        )
      )
    any_missing_data <- cols_not_enough_data %>%
      summarize(across(all_of(.env$col_names), all)) %>%
      all()
    cols_not_enough_data <- cols_not_enough_data %>%
      summarise(across(all_of(.env$col_names), any), .groups = "drop") %>%
      unlist() %>%
      names(.)[.]
  }

  if (any_missing_data) {
    cli_abort(
      "The following columns don't have enough data to {train_or_predict}: {cols_not_enough_data}.",
      class = "epipredict__not_enough_data"
    )
  }
}
