# Magical ... and substitute voodoo based on
# http://adv-r.had.co.nz/Computing-on-the-language.html#substitute
# Modeled after / copied from rundel/ghclass

handle_arg_list = function(..., tests) {
  values = list(...)
  #names = names(values)
  names = eval(substitute(alist(...)))
  names = map(names, deparse)

  walk2(names, values, tests)
}

arg_is_scalar = function(...,  allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (length(value) > 1 | (!allow_null & length(value) == 0)) {
        cli_stop("Argument {.val {name}} must be of length 1.")
      }

      if (!is.null(value)) {
        if (is.na(value) & !allow_na)
          cli_stop("Argument {.val {name}} must not be a missing value ({.val {NA}}).")
      }
    }
  )
}

arg_is_lgl = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & !allow_null)))
        cli_stop("Argument {.val {name}} must be of logical type.")

      if (any(is.na(value)) & !allow_na)
        cli_stop("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !allow_empty)
        cli_stop("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_nonneg_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value >= 0) && all(value%%1 == 0)) |
            (is.null(value) & !allow_null)))
        cli_stop("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_pos_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value > 0) && all(value%%1 == 0)) |
            (is.null(value) & !allow_null)))
        cli_stop("All {.val {name}} must be whole positive number(s).")
    }
  )
}

arg_is_int = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value%%1 == 0)) |
            (is.null(value) & !allow_null)))
        cli_stop("All {.val {name}} must be whole positive number(s).")
    }
  )
}


arg_is_lgl = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = TRUE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.logical(value) | (is.null(value) & !allow_null)))
        cli_stop("Argument {.val {name}} must be of logical type.")

      if (any(is.na(value)) & !allow_na)
        cli_stop("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !allow_empty)
        cli_stop("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_probabilities = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!((is.numeric(value) && all(value >= 0) && all(value <= 1)) |
            (is.null(value) & allow_null)))
        cli_stop("All {.val {name}} must be in [0,1].")
    }
  )
}

arg_is_chr = function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!(is.character(value) | (is.null(value) & !allow_null)))
        cli_stop("Argument {.val {name}} must be of character type.")

      if (any(is.na(value)) & !allow_na)
        cli_stop("Argument {.val {name}} must not contain any missing values ({.val {NA}}).")

      if (length(value) == 0 & !(allow_empty | allow_null))
        cli_stop("Argument {.val {name}} must have length >= 1.")
    }
  )
}

arg_is_function = function(..., allow_null = FALSE) {
  handle_arg_list(
    ...,
    tests = function(name, value) {
      if (!is.function(value) | (is.null(value) & !allow_null))
        cli_stop("{value} must be a `parsnip` function.")
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
        cli_stop("{name} must be sorted in increasing order.")

    })
}
