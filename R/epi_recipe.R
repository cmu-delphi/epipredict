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
  if (is.matrix(x) || is.data.frame(x) || tibble::is_tibble(x)) {
    x <- x[1, , drop = FALSE]
  }
  cli_warn(
    "epi_recipe has been called with a non-epi_df object, returning a regular recipe. Various
    step_epi_* functions will not work."
  )
  recipes::recipe(x, ...)
}

#' @rdname epi_recipe
#' @inheritParams recipes::recipe
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
#' @inherit recipes::recipe return
#'
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-08-01") %>%
#'   dplyr::arrange(geo_value, time_value)
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   recipes::step_naomit(recipes::all_predictors()) %>%
#'   # below, `skip` means we don't do this at predict time
#'   recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
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
          "variables."
        ))
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
    var_info <- dplyr::full_join(recipes:::get_types(x), var_info, by = "variable")
    var_info$source <- "original"

    ## arrange to easy order
    var_info <- var_info %>%
      dplyr::arrange(factor(
        role,
        levels = union(
          c("predictor", "outcome", "time_value", "geo_value", "key"),
          unique(role)
        ) # anything else
      ))

    ## Return final object of class `recipe`
    out <- list(
      var_info = var_info,
      term_info = var_info,
      steps = NULL,
      template = x[1, ],
      max_time_value = max(x$time_value),
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
  data <- data[1, ]
  # check for minus:
  if (!epiprocess::is_epi_df(data)) {
    cli_warn(
      "epi_recipe has been called with a non-epi_df object, returning a regular recipe. Various
    step_epi_* functions will not work."
    )
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
  if (!rlang::is_formula(formula)) formula <- as.formula(formula)

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
#' @keywords internal
#' @export
is_epi_recipe <- function(x) {
  inherits(x, "epi_recipe")
}



#' Add an `epi_recipe` to a workflow
#'
#' @seealso [workflows::add_recipe()]
#' - `add_recipe()` specifies the terms of the model and any preprocessing that
#'   is required through the usage of a recipe.
#'
#' - `remove_recipe()` removes the recipe as well as any downstream objects
#'
#' - `update_recipe()` first removes the recipe, then replaces the previous
#' recipe with the new one.
#'
#' @details
#' `add_epi_recipe` has the same behaviour as
#' [workflows::add_recipe()] but sets a different
#' default blueprint to automatically handle [epiprocess::epi_df] data.
#'
#' @param x A `workflow` or `epi_workflow`
#'
#' @param recipe An epi recipe or recipe
#'
#' @param ... Not used
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing
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
#' library(dplyr)
#' library(recipes)
#'
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-08-01") %>%
#'   arrange(geo_value, time_value)
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
#'
#' r2 <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7)
#'
#' workflow <- update_epi_recipe(workflow, r2)
#'
#' workflow <- remove_epi_recipe(workflow)
#'
#' workflow
#'
add_epi_recipe <- function(
    x, recipe, ..., blueprint = default_epi_recipe_blueprint()) {
  workflows::add_recipe(x, recipe, ..., blueprint = blueprint)
}

#' @rdname add_epi_recipe
#' @export
remove_epi_recipe <- function(x) {
  workflows:::validate_is_workflow(x)

  if (!workflows:::has_preprocessor_recipe(x)) {
    rlang::warn("The workflow has no recipe preprocessor to remove.")
  }

  actions <- x$pre$actions
  actions[["recipe"]] <- NULL

  new_epi_workflow(
    pre = workflows:::new_stage_pre(actions = actions),
    fit = x$fit,
    post = x$post,
    trained = FALSE
  )
}

#' @rdname add_epi_recipe
#' @export
update_epi_recipe <- function(x, recipe, ..., blueprint = default_epi_recipe_blueprint()) {
  rlang::check_dots_empty()
  x <- remove_epi_recipe(x)
  add_epi_recipe(x, recipe, ..., blueprint = blueprint)
}

#' Adjust a step in an `epi_workflow` or `epi_recipe`
#'
#' Make a parameter adjustment to a step in either an
#' `epi_workflow` or `epi_recipe` object.
#'
#'
#' @details This function can either adjust a step in a `epi_recipe` object
#' or a step from a `epi_recipe` object in an `epi_workflow`. The step to be
#' adjusted is indicated by either the step number or name (if a name is used,
#' it must be unique). In either case, the argument name and update value
#' must be inputted as `...`. See the examples below for brief
#' illustrations of the different types of updates.
#'
#' @param x A `epi_workflow` or `epi_recipe` object
#' @param which_step the number or name of the step to adjust
#' @param ... Used to input a parameter adjustment
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'
#' @return `x`, updated with the adjustment to the specified `epi_recipe` step.
#'
#' @export
#' @examples
#' library(dplyr)
#' library(workflows)
#'
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' latest <- jhu %>%
#'   filter(time_value >= max(time_value) - 14)
#'
#' # Adjust `step_epi_ahead` to have an ahead value of 14
#' # in the `epi_workflow`
#' # Option 1. Using the step number:
#' wf2 <- wf %>% adjust_epi_recipe(which_step = 2, ahead = 14)
#' extract_preprocessor(wf2)
#' # Option 2. Using the step name:
#' wf3 <- wf %>% adjust_epi_recipe(which_step = "step_epi_ahead", ahead = 14)
#' extract_preprocessor(wf3)
#'
#' # Adjust `step_epi_ahead` to have an ahead value of 14
#' # in the `epi_recipe`
#' # Option 1. Using the step number
#' r2 <- r %>% adjust_epi_recipe(which_step = 2, ahead = 14)
#' r2
#' # Option 2. Using the step name
#' r3 <- r %>% adjust_epi_recipe(which_step = "step_epi_ahead", ahead = 14)
#' r3
#'
adjust_epi_recipe <- function(x, which_step, ..., blueprint = default_epi_recipe_blueprint()) {
  UseMethod("adjust_epi_recipe")
}

#' @rdname adjust_epi_recipe
#' @export
adjust_epi_recipe.epi_workflow <- function(x, which_step, ..., blueprint = default_epi_recipe_blueprint()) {
  recipe <- adjust_epi_recipe(workflows::extract_preprocessor(x), which_step, ...)

  update_epi_recipe(x, recipe, blueprint = blueprint)
}

#' @rdname adjust_epi_recipe
#' @export
adjust_epi_recipe.epi_recipe <- function(x, which_step, ..., blueprint = default_epi_recipe_blueprint()) {
  if (!(is.numeric(which_step) || is.character(which_step))) {
    cli::cli_abort(
      c("`which_step` must be a number or a character.",
        i = "`which_step` has class {.cls {class(which_step)[1]}}."
      )
    )
  } else if (is.numeric(which_step)) {
    x$steps[[which_step]] <- update(x$steps[[which_step]], ...)
  } else {
    step_names <- map_chr(x$steps, ~ attr(.x, "class")[1])
    starts_with_step <- substr(which_step, 1, 5) == "step_"
    if (!starts_with_step) which_step <- paste0("step_", which_step)

    if (!(which_step %in% step_names)) {
      cli::cli_abort(c(
        "`which_step` does not appear in the available `epi_recipe` step names. ",
        i = "The step names are {.val {step_names}}."
      ))
    }
    which_step_idx <- which(step_names == which_step)
    if (length(which_step_idx) == 1) {
      x$steps[[which_step_idx]] <- update(x$steps[[which_step_idx]], ...)
    } else {
      cli::cli_abort(c(
        "`which_step` is not unique. Matches steps: {.val {which_step_idx}}.",
        i = "Please use the step number instead for precise alterations."
      ))
    }
  }
  x
}

# unfortunately, almost everything the same as in prep.recipe except string/fctr handling
#' @export
prep.epi_recipe <- function(
    x, training = NULL, fresh = FALSE, verbose = FALSE,
    retain = TRUE, log_changes = FALSE, strings_as_factors = TRUE, ...) {
  if (is.null(training)) {
    cli::cli_warn(c(
      "!" = "No training data was supplied to {.fn prep}.",
      "!" = "Unlike a {.cls recipe}, an {.cls epi_recipe} does not ",
      "!" = "store the full template data in the object.",
      "!" = "Please supply the training data to the {.fn prep} function,",
      "!" = "to avoid addtional warning messages."
    ))
  }
  training <- recipes:::check_training_set(training, x, fresh)
  training <- epi_check_training_set(training, x)
  training <- dplyr::relocate(training, tidyselect::all_of(epi_keys(training)))
  tr_data <- recipes:::train_info(training)
  keys <- epi_keys(x)

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
    cli::cli_warn(c(
      "Since some operations have `skip = TRUE`, using ",
      "`retain = TRUE` will allow those steps results to ",
      "be accessible."
    ))
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
        arg, ". Do you want to use a tuning function such as `tune_grid()`?"
      )
      rlang::abort(msg)
    }
    note <- paste("oper", i, gsub("_", " ", class(x$steps[[i]])[1]))
    if (!x$steps[[i]]$trained | fresh) {
      if (verbose) {
        cat(note, "[training]", "\n")
      }
      before_nms <- names(training)
      before_template <- training[1, ]
      x$steps[[i]] <- prep(x$steps[[i]],
        training = training,
        info = x$term_info
      )
      training <- bake(x$steps[[i]], new_data = training)
      if (!tibble::is_tibble(training)) {
        cli::cli_abort("`bake()` methods should always return {.cls tibble}.")
      }
      if (!is_epi_df(training)) {
        # tidymodels killed our class
        # for now, we only allow step_epi_* to alter the metadata
        training <- dplyr::dplyr_reconstruct(
          epiprocess::as_epi_df(training), before_template
        )
      }
      training <- dplyr::relocate(training, tidyselect::all_of(epi_keys(training)))
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
        dplyr::mutate(x$term_info, number = i, skip = x$steps[[i]]$skip)
      )
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
      cat(
        "The retained training set is ~",
        format(utils::object.size(training), units = "Mb", digits = 2),
        " in memory.\n\n"
      )
    }
    x$template <- training
  } else {
    x$template <- training[0, ]
  }
  x$max_time_value <- max(training$time_value)
  x$tr_info <- tr_data
  x$levels <- lvls
  x$orig_lvls <- orig_lvls
  x$retained <- retain
  x$last_term_info <- running_info %>%
    dplyr::group_by(variable) %>%
    dplyr::arrange(dplyr::desc(number)) %>%
    dplyr::summarise(
      type = list(dplyr::first(type)),
      role = list(unique(unlist(role))),
      source = dplyr::first(source),
      number = dplyr::first(number),
      skip = dplyr::first(skip),
      .groups = "keep"
    )
  x
}

#' @export
bake.epi_recipe <- function(object, new_data, ..., composition = "epi_df") {
  meta <- NULL
  if (composition == "epi_df") {
    if (is_epi_df(new_data)) {
      meta <- attr(new_data, "metadata")
    } else if (is_epi_df(object$template)) {
      meta <- attr(object$template, "metadata")
    }
    composition <- "tibble"
  }
  new_data <- NextMethod("bake")
  if (!is.null(meta)) {
    new_data <- as_epi_df(
      new_data, meta$geo_type, meta$time_type, meta$as_of,
      meta$additional_metadata %||% list()
    )
  }
  new_data
}


kill_levels <- function(x, keys) {
  for (i in which(names(x) %in% keys)) x[[i]] <- list(values = NA, ordered = NA)
  x
}

#' @export
print.epi_recipe <- function(x, form_width = 30, ...) {
  cli::cli_div(theme = list(.pkg = list("vec-trunc" = Inf, "vec-last" = ", ")))

  cli::cli_h1("Epi Recipe")
  cli::cli_h3("Inputs")

  tab <- table(x$var_info$role, useNA = "ifany")
  tab <- stats::setNames(tab, names(tab))
  names(tab)[is.na(names(tab))] <- "undeclared role"

  roles <- c("outcome", "predictor", "case_weights", "undeclared role")

  tab <- c(
    tab[names(tab) == roles[1]],
    tab[names(tab) == roles[2]],
    tab[names(tab) == roles[3]],
    sort(tab[!names(tab) %in% roles], TRUE),
    tab[names(tab) == roles[4]]
  )

  cli::cli_text("Number of variables by role")

  spaces_needed <- max(nchar(names(tab))) - nchar(names(tab)) +
    max(nchar(tab)) - nchar(tab)

  cli::cli_verbatim(
    glue::glue("{names(tab)}: {strrep('\ua0', spaces_needed)}{tab}")
  )

  if ("tr_info" %in% names(x)) {
    cli::cli_h3("Training information")
    nmiss <- x$tr_info$nrows - x$tr_info$ncomplete
    nrows <- x$tr_info$nrows

    cli::cli_text(
      "Training data contained {nrows} data points and {cli::no(nmiss)} \\
       incomplete row{?s}."
    )
  }

  if (!is.null(x$steps)) {
    cli::cli_h3("Operations")
  }

  fmt <- cli::cli_fmt({
    for (step in x$steps) {
      print(step, form_width = form_width)
    }
  })
  cli::cli_ol(fmt)
  cli::cli_end()

  invisible(x)
}

# Currently only used in the workflow printing
print_preprocessor_recipe <- function(x, ...) {
  recipe <- workflows::extract_preprocessor(x)
  steps <- recipe$steps
  n_steps <- length(steps)
  cli::cli_text("{n_steps} Recipe step{?s}.")

  if (n_steps == 0L) {
    return(invisible(x))
  }

  step_names <- map_chr(steps, workflows:::pull_step_name)

  if (n_steps <= 10L) {
    cli::cli_ol(step_names)
    return(invisible(x))
  }

  extra_steps <- n_steps - 10L
  step_names <- step_names[1:10]

  cli::cli_ol(step_names)
  cli::cli_bullets("... and {extra_steps} more step{?s}.")
  invisible(x)
}

print_preprocessor <- function(x) {
  has_preprocessor_formula <- workflows:::has_preprocessor_formula(x)
  has_preprocessor_recipe <- workflows:::has_preprocessor_recipe(x)
  has_preprocessor_variables <- workflows:::has_preprocessor_variables(x)

  no_preprocessor <- !has_preprocessor_formula && !has_preprocessor_recipe &&
    !has_preprocessor_variables

  if (no_preprocessor) {
    return(invisible(x))
  }

  cli::cli_rule("Preprocessor")
  cli::cli_text("")

  if (has_preprocessor_formula) {
    workflows:::print_preprocessor_formula(x)
  }
  if (has_preprocessor_recipe) {
    print_preprocessor_recipe(x)
  }
  if (has_preprocessor_variables) {
    workflows:::print_preprocessor_variables(x)
  }
  invisible(x)
}
