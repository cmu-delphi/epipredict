# Magical ... and substitute voodoo based on
# http://adv-r.had.co.nz/Computing-on-the-language.html#substitute
# Modeled after / copied from rundel/ghclass

handle_arg_list = function(..., tests) {
  values = list(...)
  names = eval(substitute(alist(...)))
  names = map(names, deparse)

  walk2(names, values, tests)
}

arg_is_scalar = function(...,  allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (length(value) > 1 | (!allow_null & length(value) == 0))
        cli::cli_abort(
          "Argument {.val {name}} must be of length 1.")
      if (!is.null(value)) {
        if (is.na(value) & !allow_na) {
          cli::cli_abort(
            "Argument {.val {name}} must not be a missing value ({.val {NA}})."
          )
        }
      }
    }
  )
}

arg_is_lgl = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & !allow_null))) {
        cli::cli_abort("Argument {.val {name}} must be of logical type.")
      }
      if (any(is.na(value)) & !allow_na) {
        cli::cli_abort(
          "Argument {.val {name}} must not contain any missing values ({.val {NA}})."
        )
      }
      if (length(value) == 0 & !allow_empty) {
        cli::cli_abort("Argument {.val {name}} must have length >= 1.")
      }
    }
  )
}

arg_is_lgl_scalar <- function(
    ...,
    allow_null = FALSE,
    allow_na = FALSE,
    allow_empty = TRUE) {
  arg_is_lgl(..., allow_null = allow_null, allow_na = allow_na,
             allow_empty = allow_empty)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}

arg_is_nonneg_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value >= 0) && all(value %% 1 == 0)) |
            (is.null(value) & !allow_null)))
        cli::cli_abort("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_pos = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value > 0)) |
            (is.null(value) & allow_null)))
        cli::cli_abort("All {.val {name}} must be positive number(s).")
    }
  )
}


arg_is_pos_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value > 0) && all(value %% 1 == 0)) |
            (is.null(value) & allow_null)))
        cli::cli_abort("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value %% 1 == 0)) |
            (is.null(value) & !allow_null)))
        cli::cli_abort("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_numeric = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.numeric(value) | (is.null(value) & !allow_null)))
        cli::cli_abort("All {.val {name}} must numeric.")
    }
  )
}


arg_is_lgl = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & !allow_null)))
        cli::cli_abort("Argument {.val {name}} must be of logical type.")

      if (any(is.na(value)) & !allow_na)
        cli::cli_abort("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !allow_empty)
        cli::cli_abort("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_date = function(..., allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is(value, "Date") | (is.null(value) & allow_null)))
        cli::cli_abort("Argument {.val {name}} must be a Date. Try `as.Date()`.")

      if (any(is.na(value)) & !allow_na)
        cli::cli_abort("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")
    }
  )
}

arg_is_probabilities = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value >= 0) && all(value <= 1)) |
            (is.null(value) & allow_null)))
        cli::cli_abort("All {.val {name}} must be in [0,1].")
    }
  )
}

arg_is_chr = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.character(value) | (is.null(value) & allow_null)))
        cli::cli_abort("Argument {.val {name}} must be of character type.")

      if (any(is.na(value)) & !allow_na)
        cli::cli_abort("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !(allow_empty | allow_null))
        cli::cli_abort("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_function = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!is.function(value) | (is.null(value) & !allow_null))
        cli::cli_abort("{value} must be a `parsnip` function.")
    }
  )
}

arg_is_chr_scalar = function(..., allow_null = FALSE, allow_na = FALSE) {
  arg_is_chr(..., allow_null = allow_null, allow_na = allow_na)
  arg_is_scalar(..., allow_null = allow_null, allow_na = allow_na)
}


arg_is_sorted = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (is.unsorted(value, na.rm = TRUE) | (is.null(value) & !allow_null))
        cli::cli_abort("{name} must be sorted in increasing order.")

    })
}


arg_to_date <- function(x, allow_null = FALSE, allow_na = FALSE) {
  arg_is_scalar(x, allow_null = allow_null, allow_na = allow_na)
  if (allow_null && !is.null(x)) {
    x <- tryCatch(as.Date(x), error = function(e) NA)
  }
  arg_is_date(x, allow_null = allow_null, allow_na = allow_na)
  x
}
