#' Create a epi_recipe for preprocessing data
#'
#' A recipe is a description of the steps to be applied to a data set in
#'   order to prepare it for data analysis. This is a loose wrapper
#'   around [recipes::recipe()] to properly handle the additional
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
#'
#' @export
#' @examples
#' library(epiprocess)
#' library(dplyr)
#' library(recipes)
#'
#' jhu <- jhu_csse_daily_subset %>%
#'   filter(time_value > "2021-08-01") %>%
#'   select(geo_value:death_rate_7d_av) %>%
#'   rename(case_rate = case_rate_7d_av, death_rate = death_rate_7d_av)
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_naomit(all_predictors()) %>%
#'   # below, `skip` means we don't do this at predict time
#'   step_naomit(all_outcomes(), skip = TRUE)
#'
#' r
epi_recipe.epi_df <-
  function(x, formula = NULL, ..., vars = NULL, roles = NULL) {
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
        rlang::abort(c(
            "The number of roles should be the same as the number of ",
            "variables."))
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
    class(out) <- c("epi_recipe", "recipe")
    out
  }


#' @rdname epi_recipe
#' @importFrom rlang abort
#' @export
epi_recipe.formula <- function(formula, data, ...) {
  # we ensure that there's only 1 row in the template
  data <- data[1,]
  # check for minus:
  if (! epiprocess::is_epi_df(data)) {
    return(recipes::recipe(formula, data, ...))
  }

  f_funcs <- recipes:::fun_calls(formula)
  if (any(f_funcs == "-")) {
    abort("`-` is not allowed in a recipe formula. Use `step_rm()` instead.")
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
  recipes:::inline_check(formula)

  ## use rlang to get both sides of the formula
  outcomes <- recipes:::get_lhs_vars(formula, data)
  predictors <- recipes:::get_rhs_vars(formula, data, no_lhs = TRUE)
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



#' Test for `epi_recipe`
#'
#' @param x An object.
#' @return `TRUE` if the object inherits from `epi_recipe`.
#'
#' @export
is_epi_recipe <- function(x) {
  inherits(x, "epi_recipe")
}



#' Add an epi_recipe to a workflow
#'
#' @seealso [workflows::add_recipe()]
#' - `add_recipe()` specifies the terms of the model and any preprocessing that
#'   is required through the usage of a recipe.
#'
#' - `remove_recipe()` removes the recipe as well as any downstream objects
#'
#' @details
#' Has the same behaviour as [workflows::add_recipe()] but sets a different
#' default blueprint to automatically handle [epiprocess::epi_df] data.
#'
#' @param x A workflow or epi_workflow
#'
#' @param recipe A recipe created using [recipes::recipe()]
#'
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'
#'   [default_epi_recipe_blueprint()] is used.
#'
#'   Note that preprocessing done here is separate from preprocessing that
#'   might be done automatically by the underlying model.
#'
#' @return
#' `x`, updated with a new recipe preprocessor.
#'
#' @export
#' @examples
#' library(epiprocess)
#' library(dplyr)
#' library(recipes)
#'
#' jhu <- jhu_csse_daily_subset %>%
#'   filter(time_value > "2021-08-01") %>%
#'   select(geo_value:death_rate_7d_av) %>%
#'   rename(case_rate = case_rate_7d_av, death_rate = death_rate_7d_av)
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_naomit(all_predictors()) %>%
#'   step_naomit(all_outcomes(), skip = TRUE)
#'
#' workflow <- epi_workflow() %>%
#'   add_epi_recipe(r)
#'
#' workflow
add_epi_recipe <- function(
    x, recipe, ..., blueprint = default_epi_recipe_blueprint()) {
  workflows::add_recipe(x, recipe, ..., blueprint = blueprint)
}



#' Recipe blueprint that accounts for `epi_df` panel data
#'
#' Used for simplicity. See [hardhat::default_recipe_blueprint()] for more
#' details.
#'
#' @inheritParams hardhat::default_recipe_blueprint
#'
#' @details The `bake_dependent_roles` are automatically set to `epi_df` defaults.
#' @return A recipe blueprint.
#' @export
default_epi_recipe_blueprint <-
  function(intercept = FALSE, allow_novel_levels = FALSE, fresh = TRUE,
           bake_dependent_roles = c("time_value", "geo_value", "key", "raw"),
           composition = "tibble") {
    hardhat::default_recipe_blueprint(
      intercept, allow_novel_levels, fresh, bake_dependent_roles, composition)
  }


# unfortunately, everything the same as in prep.recipe except string/fctr handling
#' @export
prep.epi_recipe <- function(
    x, training = NULL, fresh = FALSE, verbose = FALSE,
    retain = TRUE, log_changes = FALSE, strings_as_factors = TRUE, ...) {
  training <- recipes:::check_training_set(training, x, fresh)
  tr_data <- recipes:::train_info(training)
  keys <- epi_keys(training)
  orig_lvls <- lapply(training, recipes:::get_levels)
  orig_lvls <- kill_levels(orig_lvls, keys)
  if (strings_as_factors) {
    lvls <- lapply(training, recipes:::get_levels)
    lvls <- kill_levels(lvls, keys)
    training <- recipes:::strings2factors(training, lvls)
  } else {
    lvls <- NULL
  }
  skippers <- map_lgl(x$steps, recipes:::is_skipable)
  if (any(skippers) & !retain) {
    rlang::warn(c("Since some operations have `skip = TRUE`, using ",
                  "`retain = TRUE` will allow those steps results to ",
                  "be accessible."))
  }
  if (fresh) x$term_info <- x$var_info

  running_info <- x$term_info %>% dplyr::mutate(number = 0, skip = FALSE)
  for (i in seq(along.with = x$steps)) {
    needs_tuning <- map_lgl(x$steps[[i]], recipes:::is_tune)
    if (any(needs_tuning)) {
      arg <- names(needs_tuning)[needs_tuning]
      arg <- paste0("'", arg, "'", collapse = ", ")
      msg <- paste0(
        "You cannot `prep()` a tuneable recipe. Argument(s) with `tune()`: ",
        arg, ". Do you want to use a tuning function such as `tune_grid()`?")
      rlang::abort(msg)
    }
    note <- paste("oper", i, gsub("_", " ", class(x$steps[[i]])[1]))
    if (!x$steps[[i]]$trained | fresh) {
      if (verbose) {
        cat(note, "[training]", "\n")
      }
      before_nms <- names(training)
      x$steps[[i]] <- prep(x$steps[[i]], training = training,
                           info = x$term_info)
      training <- bake(x$steps[[i]], new_data = training)
      if (!tibble::is_tibble(training)) {
        abort("bake() methods should always return tibbles")
      }
      x$term_info <- recipes:::merge_term_info(get_types(training), x$term_info)
      if (!is.na(x$steps[[i]]$role)) {
        new_vars <- setdiff(x$term_info$variable, running_info$variable)
        pos_new_var <- x$term_info$variable %in% new_vars
        pos_new_and_na_role <- pos_new_var & is.na(x$term_info$role)
        pos_new_and_na_source <- pos_new_var & is.na(x$term_info$source)
        x$term_info$role[pos_new_and_na_role] <- x$steps[[i]]$role
        x$term_info$source[pos_new_and_na_source] <- "derived"
      }
      recipes:::changelog(log_changes, before_nms, names(training), x$steps[[i]])
      running_info <- rbind(
        running_info,
        dplyr::mutate(x$term_info, number = i, skip = x$steps[[i]]$skip))
    } else {
      if (verbose) cat(note, "[pre-trained]\n")
    }
  }
  if (strings_as_factors) {
    lvls <- lapply(training, recipes:::get_levels)
    lvls <- kill_levels(lvls, keys)
    check_lvls <- recipes:::has_lvls(lvls)
    if (!any(check_lvls)) lvls <- NULL
  } else {
    lvls <- NULL
  }
  if (retain) {
    if (verbose) {
      cat("The retained training set is ~",
          format(object.size(training), units = "Mb", digits = 2),
          " in memory.\n\n")
    }
    x$template <- training
  } else {
    x$template <- training[0, ]
  }
  x$tr_info <- tr_data
  x$levels <- lvls
  x$orig_lvls <- orig_lvls
  x$retained <- retain
  x$last_term_info <- running_info %>%
    dplyr::group_by(variable) %>%
    dplyr::arrange(dplyr::desc(number)) %>%
    dplyr::summarise(
      type = dplyr::first(type),
      role = as.list(unique(unlist(role))),
      source = dplyr::first(source),
      number = dplyr::first(number),
      skip = dplyr::first(skip),
      .groups = "keep")
  x
}

kill_levels <- function(x, keys) {
  for (i in which(names(x) %in% keys)) x[[i]] <- list(values = NA, ordered = NA)
  x
}

#' @export
as_tibble.epi_df <- function(x, ...) {
  # so that downstream calls to as_tibble don't clobber our metadata
  return(x)
}
