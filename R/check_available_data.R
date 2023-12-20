check_available_data <- function(
  recipe,
  ...,
  role = NA,
  trained = FALSE,
  columns = NULL,
  skip = FALSE,
  epi_keys = NULL,
  min_required_train = NULL,
  min_required_test = NULL,
  id = rand_id("available_data")) {

  arg_is_lgl_scalar(trained, skip)
  arg_is_scalar(id)
  arg_is_int(min_required_train, min_required_test, allow_null = TRUE)
  arg_is_scalar(min_required_train, min_required_test, allow_null = TRUE)
  arg_is_chr(id)
  arg_is_chr(epi_keys, allow_null = TRUE)

  if (is.null(min_required_train) || is.null(min_required_test)) {
    max_lags <- max(map_dbl(recipe$steps, ~ max(.x$lag %||% 0)), 0)
    max_horizon <- max(map_dbl(recipe$steps, ~ max(.x$horizon %||% 0)), 0)
    max_aheads <- max(map_dbl(recipe$steps, ~ max(.x$ahead %||% 0)), 0)
    computed_train_min <- max_lags + max_horizon + max_aheads
    computed_test_min <- min_required_test %||% max_lags + max_horizon
  }

  add_check(
    recipe,
    check_available_data_new(
      terms = enquos(...),
      role = role,
      trained = trained,
      columns = columns,
      skip = skip,
      min_required_train = min_required_train %||% computed_train_min,
      min_required_test = min_required_test %||% computed_test_min,
      epi_keys = epi_keys,
      id = id
    )
  )
}


check_available_data_new <- function(
    terms, role, trained, columns, skip, epi_keys, id) {
  check(
    subclass = "available_data",
    terms = terms, role = role, trained = trained,
    columns = columns, skip = skip,
    min_required_train = min_required_train,
    min_required_test = min_required_test,
    epi_keys = epi_keys, id = id
  )
}

#' @export
prep.check_available_data <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)

  ek <- x$epi_keys %||% kill_time_value(epi_keys(training))
  n_avail <- training %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(ek))) %>%
    dplyr::summarise(n = dplyr::n())

  if (any(n_avail$n < x$min_required_train)) {
    grps <- n_avail %>%
      dplyr::select(tidyselect::all_of(ek)) %>%
      as.list() %>%
      unname()
    n_avail <- n_avail$n
    names(n_avail) <- rlang::exec(paste, !!!grps, sep = " / ")
    with_too_few <- (n_avail[n_avail < x$min_required_train])
    mrt <- x$min_required_train
    cli::cli_abort(c(
      "At least {.val {mrt}} observations are required for training, but",
      "the following keys do not have enough data for training: ",
      "{.val {names(with_too_few)}}."
    ))
  }

  check_missing_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    columns = col_names,
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.check_missing <- function(object, new_data, ...) {
  col_names <- object$columns
  check_new_data(col_names, object, new_data)

  subset_to_check <- new_data[col_names]
  nr_na <- colSums(is.na(subset_to_check))
  if (any(nr_na > 0)) {
    with_na <- names(nr_na[nr_na > 0])
    cli::cli_abort(
      "The following columns contains missing values: {with_na}."
    )
  }
  new_data
}

print.check_missing <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Check missing values for "
    print_step(x$columns, x$terms, x$trained, title, width)
    invisible(x)
  }

#' @rdname tidy.recipe
#' @export
tidy.check_missing <- function(x, ...) {
  if (is_trained(x)) {
    res <- tibble(terms = unname(x$columns))
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res$id <- x$id
  res
}
