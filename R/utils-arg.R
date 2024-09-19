# Magical ... and substitute voodoo based on
# http://adv-r.had.co.nz/Computing-on-the-language.html#substitute
# Modeled after / copied from rundel/ghclass

handle_arg_list <- function(..., .tests) {
  values <- list(...)
  names <- eval(substitute(alist(...)))
  names <- map(names, deparse)

  walk2(names, values, .tests)
}

arg_is_scalar <- function(..., allow_null = FALSE, allow_na = FALSE,
                          call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_scalar(value, null.ok = allow_null, na.ok = allow_na)
    if (!ok) {
      cli_abort("{.arg {name}} must be a scalar.", call = call)
    }
  })
}

arg_is_lgl <- function(..., allow_null = FALSE, allow_na = FALSE,
                       allow_empty = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_logical(value, null.ok = allow_null, any.missing = allow_na,
                       min.len = as.integer(!allow_empty))
    if (!ok) {
      cli_abort("{.arg {name}} must be of type {.cls logical}.", call = call)
    }
  })
}

arg_is_lgl_scalar <- function(..., allow_null = FALSE, allow_na = FALSE,
                              call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_logical(value, null.ok = allow_null, any.missing = allow_na,
                       min.len = 1, max.len = 1)
    if (!ok) {
      cli_abort(
        "{.arg {name}} must be a scalar of type {.cls logical}.",
        call = call
      )
    }
  })
}

arg_is_numeric <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_numeric(value, null.ok = allow_null, any.missing = FALSE)
    if (!ok) {
      cli_abort("{.arg {name}} must be of type {.cls numeric}.", call = call)
    }
  })
}

arg_is_pos <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_numeric(
      value, lower = .Machine$double.eps,
      null.ok = allow_null, any.missing = FALSE
    )
    if (!ok) {
      len <- length(value)
      cli_abort(
        "{.arg {name}} must be {cli::qty(len)} {?a/} strictly positive number{?s}.",
        call = call
      )
    }
  })
}

arg_is_nonneg <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_numeric(value, lower = 0, null.ok = allow_null, any.missing = FALSE)
    if (!ok) {
      len <- length(value)
      cli_abort(
        "{.arg {name}} must be {cli::qty(len)} {?a/} non-negative number{?s}.",
        call = call
      )
    }
  })
}

arg_is_int <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_integerish(value, null.ok = allow_null)
    if (!ok) {
      len <- length(value)
      cli_abort(
        "{.arg {name}} must be {cli::qty(len)} {?a/} integer{?s}.",
        call = call
      )
    }
  })
}

arg_is_pos_int <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_integerish(value, null.ok = allow_null, lower = 1, any.missing = FALSE)
    if (!ok) {
      len <- length(value)
      cli_abort(
        "{.arg {name}} must be {cli::qty(len)} {?a/} positive integer{?s}.",
        call = call
      )
    }
  })
}

arg_is_nonneg_int <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_integerish(value, null.ok = allow_null, lower = 0, any.missing = FALSE)
    if (!ok) {
      len <- length(value)
      cli_abort(
        "{.arg {name}} must be {cli::qty(len)} {?a/} non-negative integer{?s}.",
        call = call
      )
    }
  })
}

arg_is_date <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_date(value, null.ok = allow_null)
    if (!ok) {
      len <- length(value)
      cli_abort(
        "{.arg {name}} must be {cli::qty(len)} {?a/} date{?s}.",
        call = call
      )
    }
  })
}

arg_is_probabilities <- function(..., allow_null = FALSE, allow_na = FALSE,
                                 call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_numeric(value, lower = 0, upper = 1, null.ok = allow_null,
                       any.missing = allow_na)
    if (!ok) {
      cli_abort("{.arg {name}} must lie in [0, 1].", call = call)
    }
  })
}

arg_is_chr <- function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = FALSE,
                       call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_character(value, null.ok = allow_null, any.missing = allow_na,
                         min.len = as.integer(!allow_empty))
    if (!ok) {
      cli_abort("{.arg {name}} must be of type {.cls character}.", call = call)
    }
  })
}

arg_is_chr_scalar <- function(..., allow_null = FALSE, allow_na = FALSE,
                              call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_character(value, null.ok = allow_null, any.missing = allow_na,
                         len = 1L)
    if (!ok) {
      cli_abort(
        "{.arg {name}} must be a scalar of type {.cls character}.",
        call = call)
    }
  })
}

arg_is_function <- function(..., allow_null = FALSE, call = caller_env()) {
  handle_arg_list(..., .tests = function(name, value) {
    ok <- test_function(value, null.ok = allow_null)
    if (!ok) {
      cli_abort("{.arg {name}} must be of type {.cls function}.", call = call)
    }
  })
}

arg_to_date <- function(x, allow_null = FALSE) {
  arg_is_scalar(x, allow_null = allow_null)
  if (!is.null(x)) {
    x <- tryCatch(as.Date(x, origin = "1970-01-01"), error = function(e) NA)
  }
  arg_is_date(x, allow_null = allow_null)
  x
}
