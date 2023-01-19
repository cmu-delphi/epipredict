#' Add layer to a frosting object
#'
#' @param frosting a `frosting` postprocessor
#' @param object a `frosting` layer
#'
#' @return an updated `frosting` postprocessor
#' @export
add_layer <- function(frosting, object) {
  validate_frosting(frosting)
  validate_layer(object)

  frosting$layers[[length(frosting$layers) + 1]] <- object

  frosting
}

## Overall Wrappers to Make New `layer_X`  Objects
#'
#' `layer` sets the class of the `layer`
#'
#' @param subclass A character string for the resulting class. For example,
#'   if `subclass = "blah"` the layer object that is returned has class
#'   `layer_blah`.
#' @param ... All arguments to the operator that should be returned.
#' @param .prefix Prefix to the subclass created.
#' @keywords internal
#' @return An updated layer with the new class
#' @export
layer <- function(subclass, ..., .prefix = "layer_") {
  structure(list(...), class = c(paste0(.prefix, subclass), "layer"))
}



#' Extract, validate, or detect layers of frosting
#'
#' These functions are mainly internal. They can access and validate
#' different layers of `frosting`.
#'
#' @param x an `epi_workflow`, `frosting`, or `layer` object
#' @param ... additional arguments for possible future methods
#' @param arg the name of the input (for error reporting)
#' @param call the environment (for error reporting)
#' @param name a layer name to detect
#'
#' @return A logical for the validators/detectors or a list of layers for
#'   the extractors
#'
#' @name layer-processors
#' @examples
#'
#' f <- frosting() %>% layer_predict()
#' wf <- epi_workflow(postprocessor = f)
#'
#' is_layer(layer("what_the_what"))
#' detect_layer(f, "layer_predict")
#' detect_layer(wf, "layer_predict")
#'
#' extract_layers(f)
#' extract_layers(wf)
NULL

#' @export
#' @rdname layer-processors
is_layer <- function(x) {
  inherits(x, "layer")
}

pull_layer_name <- function(x) {
  step <- class(x)[[1]]
  glue::glue("{step}()")
}

#' @export
#' @rdname layer-processors
validate_layer <- function(x, ..., arg = "`x`", call = caller_env()) {
  rlang::check_dots_empty()
  if (!is_layer(x)) {
    glubort(
      "{arg} must be a frosting layer, not a {class(x)[[1]]}.",
      .call = call
    )
  }
  invisible(x)
}

#' @export
#' @rdname layer-processors
detect_layer <- function(x, name, ...) {
  UseMethod("detect_layer")
}

#' @export
#' @rdname layer-processors
detect_layer.frosting <- function(x, name, ...) {
  name %in% map_chr(x$layers, ~ class(.x)[1])
}

#' @export
#' @rdname layer-processors
detect_layer.workflow <- function(x, name, ...) {
  validate_has_postprocessor(x)
  detect_layer(x$post$actions$frosting$frosting, name)
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
#' @param the_recipe the `epi_recipe` preprocessor
#'
#' @param ... additional arguments used by methods. Currently unused.
#'
#' @return The `components` list. In the same format after applying any updates.
#' @export
slather <- function(object, components, the_fit, the_recipe, ...) {
  UseMethod("slather")
}

# Possible types of steps
# 1. mutate-like (apply a scalar transform to the .preds)
# 2. Filtering (remove some rows from .preds)
# 3. Imputation (fill the NA's in .preds, might need the training data)
# 4. Quantiles (needs the_fit, residuals, .preds)
# 5. add_target_date (needs the recipe, training data?)
# requirements = c("predictions", "fit", "predictors", "outcomes", "extras"),
