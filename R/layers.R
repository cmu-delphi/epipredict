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

#' Update post-processing `layer`
#'
#' This `layer` method for `update()` takes named arguments as `...` whose values
#' will replace the elements of the same name in the actual post-processing layer.
#' Analogous to `update.step()` from the `recipes` package.
#'
#' @param object A post-processing `layer`.
#' @param ... Key-value pairs where the keys match up with names of elements
#' in the layer, and the values are the new values to update the layer with.
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#' dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' latest <- jhu %>%
#'   dplyr::filter(time_value >= max(time_value) - 14)
#'
#' # Specify a `forecast_date` that is greater than or equal to `as_of` date
#' f <- frosting() %>% layer_predict() %>%
#'   layer_add_forecast_date(forecast_date = "2022-05-31") %>%
#'   layer_naomit(.pred)
#'
#' wf1 <- wf %>% add_frosting(f)
#'
#' p1 <- predict(wf1, latest)
#' p1
#'
#' # Update forecast date
#' f$layers[[2]] <- update(f$layers[[2]], forecast_date = "2021-06-01")
#'
#' # Need to still update workflow if only update a layer in frosting
#' wf2 <- wf %>% add_frosting(f)
#' wf2$post # Check that wf1 has update
#' p1 <- predict(wf2, latest)
#' p1
#' @export
update.layer <- function(object, ...) {
  changes <- list(...)

  # Replace the appropriate values in object with the changes
  object <- recipes:::update_fields(object, changes)

  # Call layer() to construct a new layer to ensure all new changes are validated
  reconstruct_layer(object)
}

reconstruct_layer <- function(x) {

  # Collect the subclass of the layer to use
  # when recreating it
  subclass <- setdiff(class(x), "layer")

  # A layer is just a list of its arguments
  args <- unclass(x)

  # Construct the call and splice in the args
  # no prefix is needed because we know the full subclass
  call_layer <- rlang::call2(
    .fn = "layer",
    subclass = subclass,
    !!!args,
    .prefix = "",
    .ns = "epipredict"
  )

  rlang::eval_tidy(call_layer)
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
