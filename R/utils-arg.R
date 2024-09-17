# Magical ... and substitute voodoo based on
# http://adv-r.had.co.nz/Computing-on-the-language.html#substitute
# Modeled after / copied from rundel/ghclass

handle_arg_list <- function(..., .tests) {
  values <- list(...)
  names <- eval(substitute(alist(...)))
  names <- map(names, deparse)

  walk2(names, values, .tests)
}

arg_is_scalar <- function(..., allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_scalar(value, null.ok = allow_null, na.ok = allow_na, .var.name = name)
  })
}

arg_is_lgl <- function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_logical(value, null.ok = allow_null, any.missing = allow_na, min.len = as.integer(!allow_empty), .var.name = name)
  })
}

arg_is_lgl_scalar <- function(..., allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_logical(value, null.ok = allow_null, any.missing = allow_na, min.len = 1, max.len = 1, .var.name = name)
  })
}

arg_is_numeric <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_numeric(value, null.ok = allow_null, any.missing = FALSE, .var.name = name)
  })
}

arg_is_pos <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_numeric(value, lower = 1, null.ok = allow_null, any.missing = FALSE, .var.name = name)
  })
}

arg_is_nonneg <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_numeric(value, lower = 0, null.ok = allow_null, any.missing = FALSE, .var.name = name)
  })
}

arg_is_int <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_integerish(value, null.ok = allow_null, .var.name = name)
  })
}

arg_is_pos_int <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_integerish(value, null.ok = allow_null, lower = 1, any.missing = FALSE, .var.name = name)
  })
}

arg_is_nonneg_int <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_integerish(value, null.ok = allow_null, lower = 0, any.missing = FALSE, .var.name = name)
  })
}

arg_is_date <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_date(value, null.ok = allow_null, .var.name = name)
  })
}

arg_is_probabilities <- function(..., allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_numeric(value, lower = 0, upper = 1, null.ok = allow_null, any.missing = allow_na, .var.name = name)
  })
}

arg_is_chr <- function(..., allow_null = FALSE, allow_na = FALSE, allow_empty = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_character(value, null.ok = allow_null, any.missing = allow_na, min.len = as.integer(!allow_empty), .var.name = name)
  })
}

arg_is_chr_scalar <- function(..., allow_null = FALSE, allow_na = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_character(value, null.ok = allow_null, any.missing = allow_na, min.len = 1, max.len = 1, .var.name = name)
  })
}

arg_is_function <- function(..., allow_null = FALSE) {
  handle_arg_list(..., .tests = function(name, value) {
    assert_function(value, null.ok = allow_null, .var.name = name)
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

check_tidyselect_cols_exist <- function(selection, data, call = caller_env()) {
  name_pos <- tidyselect::eval_select(selection, data, error_call = call)
  if (length(name_pos) == 0L) {
    return(list(ok = TRUE, missing_names = selection))
  }
  hardhat::check_column_names(data, names(name_pos))
}

validate_tidyselect_cols_exist <- function(selection, data, call = caller_env()) {
  check <- check_tidyselect_cols_exist(selection, data, call)
  if (!check$ok) {
    missing_names <- glue::glue_collapse(
      glue::single_quote(check$missing_names),
      sep = ", "
    )
    message <- glue::glue(
      "The {selection} results in missing columns: {missing_names}."
    )
    cli_abort(message, call = call)
  }
  invisible(data)
}
