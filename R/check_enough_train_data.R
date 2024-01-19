#' Check the dataset contains enough data points.
#'
#' `check_enough_train_data` creates a *specification* of a recipe
#'  operation that will check if variables contain enough data. NA values
#'  are not counted as data points.
#'
#' @param recipe A recipe object. The check will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables
#'  for this check. See [selections()] for more details.
#' @param role Not used by this check since no new variables are
#'  created.
#' @param trained A logical for whether the selectors in `...`
#' have been resolved by [prep()].
#' @param id A character string that is unique to this check to identify it.
#' @param skip A logical. Should the check be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @family checks
#' @export
#' @details This check will break the `bake` function if any of the checked
#'  columns have not enough non-NA values. If the check passes, nothing is
#'  changed to the data.
#'
#'  # tidy() results
#'
#'  When you [`tidy()`][tidy.recipe()] this check, a tibble with column
#'  `terms` (the selectors or variables selected) is returned.
#'
#' TODO: Change these tests for check enough data.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(credit_data, package = "modeldata")
#' is.na(credit_data) %>% colSums()
#'
#' # If the test passes, `new_data` is returned unaltered
#' recipe(credit_data) %>%
#'   check_missing(Age, Expenses) %>%
#'   prep() %>%
#'   bake(credit_data)
#'
#' # If your training set doesn't pass, prep() will stop with an error
#' \dontrun{
#' recipe(credit_data) %>%
#'   check_missing(Income) %>%
#'   prep()
#' }
#'
#' # If `new_data` contain missing values, the check will stop `bake()`
#'
#' train_data <- credit_data %>% dplyr::filter(Income > 150)
#' test_data <- credit_data %>% dplyr::filter(Income <= 150 | is.na(Income))
#'
#' rp <- recipe(train_data) %>%
#'   check_missing(Income) %>%
#'   prep()
#'
#' bake(rp, train_data)
#' \dontrun{
#' bake(rp, test_data)
#' }
check_enough_train_data <-
  function(recipe,
           ...,
           n,
           epi_keys = NULL,
           drop_na = TRUE,
           role = NA,
           trained = FALSE,
           columns = NULL,
           skip = TRUE,
           id = rand_id("enough_train_data")) {
    add_check(
      recipe,
      check_enough_train_data_new(
        n = n,
        epi_keys = epi_keys,
        drop_na = drop_na,
        terms = rlang::enquos(...),
        role = role,
        trained = trained,
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

check_enough_train_data_new <-
  function(n, epi_keys, drop_na, terms, role, trained, columns, skip, id) {
    check(
      subclass = "enough_train_data",
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
prep.check_enough_train_data <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  cols_not_enough_data <- purrr::map(col_names, function(col) {
    groups_below_thresh <- training %>%
      dplyr::select(all_of(c(epi_keys(training), col))) %>%
      {
        if (x$drop_na) {
          tidyr::drop_na(.)
        } else {
          .
        }
      } %>%
      dplyr::count(dplyr::across(dplyr::all_of(x$epi_keys))) %>%
      dplyr::filter(n < x$n)
    if (nrow(groups_below_thresh) > 0) {
      col
    }
  }) %>% purrr::keep(~ !is.null(.))

  if (length(cols_not_enough_data) > 0) {
    cli::cli_abort(
      "The following columns don't have enough data to predict: {cols_not_enough_data}."
    )
  }

  check_enough_train_data_new(
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
bake.check_enough_train_data <- function(object, new_data, ...) {
  new_data
}

print.check_enough_train_data <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- paste0("Check enough data (n = ", x$n, ") for ")
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.check_enough_train_data <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res$n <- x$n
  res$epi_keys <- x$epi_keys
  res$drop_na <- x$drop_na
  res
}
