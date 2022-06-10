add_frosting <- function(x, frosting, ...) {
  rlang::check_dots_empty()
  action <- workflows:::new_action_post(frosting = frosting)
  workflows:::add_action(x, action, "frosting")
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


frosting <- function(layers = NULL, requirements = NULL) {
  if (!is_null(layers) || !is_null(requirements)) {
    rlang::abort(c("Currently, no arguments to `frosting()` are allowed",
                 "to be non-null."))
  }
  out <- new_frosting()
}

#' @export
apply_frosting <- function(workflow, ...) {
  UseMethod("apply_frosting")
}

#' @export
apply_frosting.default <- function(workflow, components, ...) {
  if (has_postprocessor(workflow)) {
    abort(c("Postprocessing is only available for epi_workflows currently.",
            i = "Can you use `epi_workflow()` instead of `workflow()`?"))
  }
  return(components)
}



#' @importFrom rlang is_null
#' @importFrom rlang abort
#' @export
apply_frosting.epi_workflow <- function(workflow, components, the_fit, ...) {
  if (!has_postprocessor(workflow)) {
    components$preds <- predict(the_fit, components$forged$predictors, ...)
    return(components)
  }
  if (!has_postprocessor_frosting(workflow)) {
    rlang::warn(c("Only postprocessors of class frosting are allowed.",
                  "Returning unpostprocessed predictions."))
    components$preds <- predict(the_fit, components$forged$predictors, ...)
    return(components)
  }
  layers <- workflow$post$actions$frosting
  for (l in seq_along(layers)) {
    layer <- layers$layers[[l]]
    components <- slather(layer, components = components, the_fit)
  }
  # last for the moment, anticipating that layer[1] will do the prediction...
  if (is_null(components$preds)) {
    components$preds <- predict(the_fit, components$forged$predictors, ...)
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

slather <- function(x, ...) {
  UseMethod("slather")
}

# Possible types of steps
# 1. mutate-like (apply a scalar transform to the .preds)
# 2. Filtering (remove some rows from .preds)
# 3. Imputation (fill the NA's in .preds, might need the training data)
# 4. Quantiles (needs the_fit, residuals, .preds)
# 5. add_target_date (needs the recipe, training data?)
# requirements = c("predictions", "fit", "predictors", "outcomes", "extras"),
