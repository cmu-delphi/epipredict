#' Create a epi_recipe for preprocessing data
#'
#' A recipe is a description of the steps to be applied to a data set in
#'   order to prepare it for data analysis. This is a loose wrapper
#'   around `recipes::recipe()` to properly handle the additional
#'   columns present in an `epi_df`
#'
#' @aliases epi_recipe epi_recipe.default epi_recipe.formula
#' @import recipes
#' @export
epi_recipe <- function(x, ...) {
  UseMethod("epi_recipe")
}


#' @rdname epi_recipe
#' @export
epi_recipe.default <- function(x, ...) {
  ## if not a formula or an epi_df, we just pass to recipes::recipe
  if (is.matrix(x) || is.data.frame(x) || tibble::is_tibble(x))
    x <- x[1,,drop=FALSE]
  recipes::recipe(x, ...)
}

#' @rdname epi_recipe
#' @param vars A character string of column names corresponding to variables
#'   that will be used in any context (see below)
#' @param roles A character string (the same length of `vars`) that
#'   describes a single role that the variable will take. This value could be
#'   anything but common roles are `"outcome"`, `"predictor"`,
#'   `"time_value"`, and `"geo_value"`
#' @param ... Further arguments passed to or from other methods (not currently
#'   used).
#' @param formula A model formula. No in-line functions should be used here
#'  (e.g. `log(x)`, `x:y`, etc.) and minus signs are not allowed. These types of
#'  transformations should be enacted using `step` functions in this package.
#'  Dots are allowed as are simple multivariate outcome terms (i.e. no need for
#'  `cbind`; see Examples).
#' @param x,data A data frame, tibble, or epi_df of the *template* data set
#'   (see below). This is always coerced to the first row to avoid memory issues
#' @return An object of class `recipe` with sub-objects:
#'   \item{var_info}{A tibble containing information about the original data
#'   set columns}
#'   \item{term_info}{A tibble that contains the current set of terms in the
#'   data set. This initially defaults to the same data contained in
#'   `var_info`.}
#'   \item{steps}{A list of `step`  or `check` objects that define the sequence of
#'   preprocessing operations that will be applied to data. The default value is
#'   `NULL`}
#'   \item{template}{A tibble of the data. This is initialized to be the same
#'   as the data given in the `data` argument but can be different after
#'   the recipe is trained.}
#'
#  @includeRmd man/rmd/recipes.Rmd details
#'
#' @export
epi_recipe.epi_df <-
  function(x,
           formula = NULL,
           ...,
           vars = NULL,
           roles = NULL) {
    if (!is.null(formula)) {
      if (!is.null(vars)) {
        rlang::abort(
          paste0(
            "This `vars` specification will be ignored ",
            "when a formula is used"
          )
        )
      }
      if (!is.null(roles)) {
        rlang::abort(
          paste0(
            "This `roles` specification will be ignored ",
            "when a formula is used"
          )
        )
      }

      obj <- epi_recipe.formula(formula, x, ...)
      return(obj)
    }
    if (is.null(vars)) vars <- colnames(x)
    if (any(table(vars) > 1)) {
      rlang::abort("`vars` should have unique members")
    }
    if (any(!(vars %in% colnames(x)))) {
      rlang::abort("1 or more elements of `vars` are not in the data")
    }

    keys <- epi_keys(x) # we know x is an epi_df

    var_info <- tibble(variable = vars)
    key_roles <- c("time_value", "geo_value", rep("key", length(keys) - 2))

    ## Check and add roles when available
    if (!is.null(roles)) {
      if (length(roles) != length(vars)) {
        rlang::abort(
          paste0(
            "The number of roles should be the same as the number of ",
            "variables"
          )
        )
      }
      var_info$role <- roles
    } else {
      var_info <- var_info %>% dplyr::filter(!(variable %in% keys))
      var_info$role <- "raw"
    }
    ## Now we add the keys when necessary
    var_info <- dplyr::union(
      var_info,
      tibble::tibble(variable = keys, role = key_roles)
    )

    ## Add types
    var_info <- dplyr::full_join(get_types(x), var_info, by = "variable")
    var_info$source <- "original"

    ## arrange to easy order
    var_info <- var_info %>%
      dplyr::arrange(factor(
        role,
        levels = union(
          c("predictor", "outcome", "time_value", "geo_value", "key"),
          unique(role)) # anything else
      ))

    ## Return final object of class `recipe`
    out <- list(
      var_info = var_info,
      term_info = var_info,
      steps = NULL,
      template = x[1,],
      levels = NULL,
      retained = NA
    )
    class(out) <- "recipe"
    out
  }


#' @rdname epi_recipe
#' @export
epi_recipe.formula <- function(formula, data, ...) {
  # we ensure that there's only 1 row in the template
  data <- data[1,]
  # check for minus:
  if (! epiprocess::is_epi_df(data)) {
    return(recipes::recipe(formula, data, ...))
  }

  f_funcs <- fun_calls(formula)
  if (any(f_funcs == "-")) {
    Abort("`-` is not allowed in a recipe formula. Use `step_rm()` instead.")
  }

  # Check for other in-line functions
  args <- epi_form2args(formula, data, ...)
  obj <- epi_recipe.epi_df(
    x = args$x,
    formula = NULL,
    ...,
    vars = args$vars,
    roles = args$roles
  )
  obj
}


# slightly modified version of `form2args()` in {recipes}
epi_form2args <- function(formula, data, ...) {
  if (! rlang::is_formula(formula)) formula <- as.formula(formula)

  ## check for in-line formulas
  inline_check(formula)

  ## use rlang to get both sides of the formula
  outcomes <- get_lhs_vars(formula, data)
  predictors <- get_rhs_vars(formula, data, no_lhs = TRUE)
  keys <- epi_keys(data)

  ## if . was used on the rhs, subtract out the outcomes
  predictors <- predictors[!(predictors %in% outcomes)]
  ## if . was used anywhere, remove epi_keys
  if (rlang::f_lhs(formula) == ".") {
    outcomes <- outcomes[!(outcomes %in% keys)]
  }
  if (rlang::f_rhs(formula) == ".") {
    predictors <- predictors[!(predictors %in% keys)]
  }

  ## get `vars` from rhs, lhs. keys get added downstream
  vars <- c(predictors, outcomes)
  ## subset data columns
  data <- data[, union(vars, keys)]

  ## derive roles
  roles <- rep("predictor", length(predictors))
  if (length(outcomes) > 0) {
    roles <- c(roles, rep("outcome", length(outcomes)))
  }
  # if (length(keys) > 0) {
  #   roles <- c(roles, c("time_value", rep("key", length(keys) - 1)))
  # }

  ## pass to recipe.default with vars and roles
  list(x = data, vars = vars, roles = roles)
}

