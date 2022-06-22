#' Add frosting to a workflow
#'
#' @param x A workflow
#' @param frosting A frosting layer created using `frosting()`
#' @param ... Not used.
#'
#' @return `x`, updated with a new or removed frosting postprocessor
#' @export
#'
#' @examples
#' #' # Try out frosting validators / constructors
#'  wf <- epi_workflow()
#' is_frosting(new_frosting()) # should be TRUE
#'
#'  ## Three ways to add a frosting postprocessor
#'  wf1 <- epi_workflow(postprocessor = new_frosting())
#'  has_postprocessor(wf1) # should be TRUE
#'  has_postprocessor_frosting(wf1) # should be TRUE
#'
#'  wf2 <- wf %>% add_postprocessor(new_frosting())
#'  has_postprocessor(wf2) # should be TRUE
#'  has_postprocessor_frosting(wf2) # should be TRUE
#'
#' wf3 <- wf %>% add_frosting(new_frosting())
#'  has_postprocessor(wf3) # should be TRUE
#'  has_postprocessor_frosting(wf3) # should be TRUE
#'
#' # Frosting can be created/added/removed
#'  f <- frosting()
#'  wf <- epi_workflow() %>% add_frosting(f)
#' has_postprocessor(wf) # should be TRUE
#' has_postprocessor_frosting(wf) # should be TRUE
#'  wf <- wf %>% remove_frosting()
#'  has_postprocessor(wf) # should be FALSE
#'  has_postprocessor_frosting(wf) # should be FALSE
add_frosting <- function(x, frosting, ...) {
  rlang::check_dots_empty()
  action <- workflows:::new_action_post(frosting = frosting)
  workflows:::add_action(x, action, "frosting")
}

#' @rdname add_frosting
#' @export
remove_frosting <- function(x) {
  workflows:::validate_is_workflow(x)

  if (!has_postprocessor_frosting(x)) {
    rlang::warn("The workflow has no frosting postprocessor to remove.")
    return(x)
  }

  x$post$actions[["frosting"]] <- NULL
  x
}


has_postprocessor_frosting <- function(x) {
  "frosting" %in% names(x$post$actions)
}

has_postprocessor <- function(x) {
  length(x$post$actions) > 0
}

validate_has_postprocessor <- function(x, ..., call = caller_env()) {
  rlang::check_dots_empty()
  has_postprocessor <- has_postprocessor_frosting(x)
  if (!has_postprocessor) {
    message <- c("The workflow must have a frosting postprocessor.",
                 i = "Provide one with `add_frosting()`.")
    rlang::abort(message, call = call)
  }
  invisible(x)
}




#' @importFrom rlang caller_env
add_postprocessor <- function(x, postprocessor, ..., call = caller_env()) {
  rlang::check_dots_empty()
  if (is_frosting(postprocessor)) {
    return(add_frosting(x, postprocessor))
  }
  rlang::abort("`postprocessor` must be a frosting object.", call = call)
}

is_frosting <- function(x) {
  inherits(x, "frosting")
}

new_frosting <- function() {
  structure(
    list(
      layers = NULL,
      requirements = NULL
    ),
    class = "frosting"
  )
}


#' Create frosting for postprocessing predictions
#'
#' This generates a postprocessing container (much like `recipes::recipe()`)
#' to hold steps for postprocessing predictions.
#'
#' The arguments are currently placeholders and must be NULL
#'
#' @param layers Must be `NULL`.
#' @param requirements Must be `NULL`.
#'
#' @return A frosting object.
#' @export
frosting <- function(layers = NULL, requirements = NULL) {
  if (!is_null(layers) || !is_null(requirements)) {
    rlang::abort(c("Currently, no arguments to `frosting()` are allowed",
                 "to be non-null."))
  }
  out <- new_frosting()
}

#' Apply postprocessing to a fitted workflow
#'
#' This function is intended for internal use. It implements postprocessing
#' inside of the `predict()` method for a fitted workflow.
#'
#' @param workflow An object of class workflow
#' @param ... additional arguments passed on to methods
#'
#' @aliases apply_frosting.default apply_frosting.epi_recipe
#' @export
apply_frosting <- function(workflow, ...) {
  UseMethod("apply_frosting")
}

#' @inheritParams slather
#' @rdname apply_frosting
#' @export
apply_frosting.default <- function(workflow, components, ...) {
  if (has_postprocessor(workflow)) {
    abort(c("Postprocessing is only available for epi_workflows currently.",
            i = "Can you use `epi_workflow()` instead of `workflow()`?"))
  }
  return(components)
}



#' @rdname apply_frosting
#' @importFrom rlang is_null
#' @importFrom rlang abort
#' @export
apply_frosting.epi_workflow <- function(workflow, components, the_fit, ...) {
  if (!has_postprocessor(workflow)) {
    components$predictions <- predict(the_fit, components$forged$predictors, ...)
    components$predictions <- dplyr::bind_cols(components$keys, components$predictions)
    return(components)
  }
  if (!has_postprocessor_frosting(workflow)) {
    rlang::warn(c("Only postprocessors of class frosting are allowed.",
                  "Returning unpostprocessed predictions."))
    components$predictions <- predict(the_fit, components$forged$predictors, ...)
    components$predictions <- dplyr::bind_cols(components$keys, components$predictions)
    return(components)
  }
  layers <- workflow$post$actions$frosting$frosting$layers
  for (l in seq_along(layers)) {
    la <- layers[[l]]
    components <- slather(la, components = components, the_fit = the_fit)
  }
  # last for the moment, anticipating that layer[1] will do the prediction...
  if (is_null(components$predictions)) {
    components$predictions <- predict(the_fit, components$forged$predictors, ...)
    components$predictions <- dplyr::bind_cols(components$keys, components$predictions)
  }
  return(components)
}


layer <- function(subclass, ..., .prefix = "layer_") {
  structure(list(...), class = c(paste0(.prefix, subclass), "layer"))
}

add_layer <- function(frosting, object) {
  frosting$layers[[length(frosting$layers) + 1]] <- object
  frosting
}


#' Spread a layer of frosting on a fitted workflow
#'
#' Slathering frosting means to implement a postprocessing layer. When
#' creating a new postprocessing layer, you must implement an S3 method
#' for this function
#'
#' @param object a workflow with `frosting` postprocessing steps
#' @param components a list of components containing model information. These
#'   will be updated and returned by the layer. These should be
#'   * `mold` - the output of calling `hardhat::mold()` on the workflow. This
#'     contains information about the preprocessing, including the recipe.
#'   * `forged` - the output of calling `hardhat::forge()` on the workflow.
#'     This should have predictors and outcomes for the `new_data`. It will
#'     have three components `predictors`, `outcomes` (if these were in the
#'     `new_data`), and `extras` (usually has the rest of the data, including
#'     `keys`).
#'   * `keys` - we put the keys (`time_value`, `geo_value`, and any others)
#'     here for ease.
#' @param the_fit the fitted model object as returned by calling `parsnip::fit()`
#'
#' @param ... additional arguments used by methods. Currently unused.
#'
#' @return The `components` list. In the same format after applying any updates.
#' @export
slather <- function(object, components, the_fit, ...) {
  UseMethod("slather")
}

# Possible types of steps
# 1. mutate-like (apply a scalar transform to the .preds)
# 2. Filtering (remove some rows from .preds)
# 3. Imputation (fill the NA's in .preds, might need the training data)
# 4. Quantiles (needs the_fit, residuals, .preds)
# 5. add_target_date (needs the recipe, training data?)
# requirements = c("predictions", "fit", "predictors", "outcomes", "extras"),
