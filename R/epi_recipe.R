#' @import recipes
#' @export
epi_recipe <- function(x, ...) {
  # deprecate_soft("This function is being deprecated. Use `recipe()` instead.")
  UseMethod("epi_recipe")
}

#' @export
epi_recipe.default <- function(x, ...) {
  recipe(x, ...)
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
#' default blueprint to automatically handle [epiprocess::epi_df][epiprocess::as_epi_df] data.
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
#' r <- recipe(jhu) %>%
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
#' r2 <- recipe(jhu) %>%
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
#' r <- recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
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


#' @export
prep.epi_recipe <- function(
    x, training = NULL, fresh = FALSE, verbose = FALSE,
    retain = TRUE, log_changes = FALSE, strings_as_factors = TRUE, ...) {

  if (!strings_as_factors) return(NextMethod("prep"))
  # workaround to avoid converting strings2factors with recipes::prep.recipe()
  # We do the conversion here, then set it to FALSE
  training <- recipes:::check_training_set(training, x, fresh)
  training <- epi_check_training_set(training, x)
  training <- dplyr::relocate(training, tidyselect::all_of(epi_keys(training)))
  keys <- epi_keys(x)
  orig_lvls <- lapply(training, recipes:::get_levels)
  orig_lvls <- kill_levels(orig_lvls, keys)
  lvls <- lapply(training, recipes:::get_levels)
  lvls <- kill_levels(lvls, keys) # don't do anything to the epi_keys
  training <- recipes:::strings2factors(training, lvls)

  # browser()
  x <- NextMethod("prep", training = training, fresh = fresh,
                  verbose = verbose,
                  retain = retain, log_changes = log_changes,
                  strings_as_factors = FALSE, ...)
  # Now, we undo the conversion.

  lvls <- lapply(x$template, recipes:::get_levels)
  lvls <- kill_levels(lvls, keys)
  check_lvls <- recipes:::has_lvls(lvls)
  if (!any(check_lvls)) lvls <- NULL
  x$levels <- lvls
  x$orig_lvls <- orig_lvls
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
    # Baking should have dropped epi_df-ness and metadata. Re-infer some
    # metadata and assume others remain the same as the object/template:
    new_data <- as_epi_df(
      new_data,
      as_of = meta$as_of,
      # avoid NULL if meta is from saved older epi_df:
      additional_metadata = meta$additional_metadata %||% list()
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
