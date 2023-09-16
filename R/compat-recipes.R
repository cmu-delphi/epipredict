# These are copied from `recipes` where they are unexported

fun_calls <- function(f) {
  if (is.function(f)) {
    fun_calls(body(f))
  } else if (rlang::is_quosure(f)) {
    fun_calls(rlang::quo_get_expr(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])
    if (identical(fname, ".Internal")) {
      return(fname)
    }
    unique(c(fname, unlist(lapply(f[-1], fun_calls), use.names = FALSE)))
  }
}

inline_check <- function(x) {
  funs <- fun_calls(x)
  funs <- funs[!(funs %in% c("~", "+", "-"))]
  if (length(funs) > 0) {
    rlang::abort(paste0(
      "No in-line functions should be used here; ",
      "use steps to define baking actions."
    ))
  }
  invisible(x)
}

#' @importFrom stats as.formula
get_lhs_vars <- function(formula, data) {
  if (!rlang::is_formula(formula)) {
    formula <- as.formula(formula)
  }
  ## Want to make sure that multiple outcomes can be expressed as
  ## additions with no cbind business and that `.` works too (maybe)
  new_formula <- rlang::new_formula(lhs = NULL, rhs = rlang::f_lhs(formula))
  get_rhs_vars(new_formula, data)
}

#' @importFrom stats model.frame
get_rhs_vars <- function(formula, data, no_lhs = FALSE) {
  if (!rlang::is_formula(formula)) {
    formula <- as.formula(formula)
  }
  if (no_lhs) {
    formula <- rlang::new_formula(lhs = NULL, rhs = rlang::f_rhs(formula))
  }

  ## This will need a lot of work to account for cases with `.`
  ## or embedded functions like `Sepal.Length + poly(Sepal.Width)`.
  ## or should it? what about Y ~ log(x)?
  ## Answer: when called from `form2args`, the function
  ## `inline_check` stops when in-line functions are used.
  data_info <- attr(model.frame(formula, data[1, ]), "terms")
  response_info <- attr(data_info, "response")
  predictor_names <- names(attr(data_info, "dataClasses"))
  if (length(response_info) > 0 && all(response_info > 0)) {
    predictor_names <- predictor_names[-response_info]
  }
  predictor_names
}

## Buckets variables into discrete, mutally exclusive types
get_types <- function(x) {
  var_types <-
    c(
      character = "nominal",
      factor = "nominal",
      ordered = "nominal",
      integer = "numeric",
      numeric = "numeric",
      double = "numeric",
      Surv = "censored",
      logical = "logical",
      Date = "date",
      POSIXct = "date",
      list = "list",
      textrecipes_tokenlist = "tokenlist"
    )

  classes <- lapply(x, class)
  res <- lapply(
    classes,
    function(x, types) {
      in_types <- x %in% names(types)
      if (sum(in_types) > 0) {
        # not sure what to do with multiple matches; right now
        ## pick the first match which favors "factor" over "ordered"
        out <- unname(types[min(which(names(types) %in% x))])
      } else {
        out <- "other"
      }
      out
    },
    types = var_types
  )
  res <- unlist(res)
  tibble(variable = names(res), type = unname(res))
}
