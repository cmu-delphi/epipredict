#' Check Training Window Length
#'
#' `check_train_window` creates a *specification* of a recipe
#'  check that will check if there is insufficient training data
#'
#' @inheritParams check_missing
#' @min_train_window Positive integer. The minimum amount of training
#'   data time points required
#'   to fit a predictive model. Using less results causes downstream
#'   fit calls to return minimal objects rather than crashing.
#' @param warn If `TRUE` the check will throw a warning instead
#'   of an error when failing.
#' @param train_window The number of days of training data.
#'   This is `NULL` until computed by [prep()].
#' @template check-return
#' @family checks
#' @export
#'
check_train_window <-
  function(recipe,
           ...,
           role = NA,
           skip = FALSE,
           trained = FALSE,
           min_train_window = 20,
           warn = TRUE,
           train_length,
           id = rand_id("train_window_check_")) {
    add_check(
      recipe,
      check_train_window_new(
        terms = dplyr::enquos(...),
        role = role,
        skip = skip,
        trained = trained,
        min_train_window = min_train_window,
        warn = warn,
        train_length = train_length,
        id = id
      )
    )
  }

## Initializes a new object
check_train_window_new <-
  function(terms, role, skip, trained, min_train_window, warn,
           train_length, id) {
    check(
      subclass = "train_window",
      terms = terms,
      role = role,
      skip = skip,
      trained = trained,
      min_train_window = min_train_window,
      warn = warn,
      train_length = train_length,
      id = id
    )
  }


prep.check_train_window <- function(x,
                                    training,
                                    info = NULL,
                                    ...) {

  train_length <- nrow(training)


  check_train_window_new(
    terms      = x$terms,
    role       = x$role,
    trained    = TRUE,
    skip       = x$skip,
    warn       = x$warn,
    min_train_window = min_train_window,
    warn = warn,
    train_length = train_length,
    id         = x$id
  )
}

bake.check_range <- function(object,
                             new_data,
                             ...) {

  mtw <- object$min_train_window
  stopifnot(is.numeric(mtw), length(mtw) == 1L, mtw == as.integer(mtw))

  n <- nrow(new_data)
  n.complete <- sum(complete.cases(new_data))

  msg <- NULL
  if (n < mtw) {
    msg <- paste0(msg, "Total available rows of data is ", n,
                  "\n < min_train_window ", mtw, ".\n")
  }
  if (n.complete < mtw) {
    msg <- paste0(msg, "Total complete rows of data is ", n.complete,
                  "\n < min_train_window ", mtw, ".\n")
  }

  if (object$warn & !is.null(msg)) {
    rlang::warn(msg)
  } else if (!is.null(msg)) {
    rlang::abort(msg)
  }

  as_tibble(new_data)
}

print.check_train_window <-
  function(x, width = max(20, options()$width - 30), ...) {
    title <- "Checking number of training observations"
    invisible(x)
  }


