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
#' library(dplyr)
#' library(recipes)
#'
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_naomit(all_predictors()) %>%
#'   step_naomit(all_outcomes(), skip = TRUE)
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% parsnip::fit(jhu)
#' latest <- jhu %>%
#'   filter(time_value >= max(time_value) - 14)
#'
#' # Add frosting to a workflow and predict
#' f <- frosting() %>% layer_predict() %>% layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#' p1 <- predict(wf1, latest)
#' p1
#'
#' # Remove frosting from the workflow and predict
#' wf2 <- wf1 %>% remove_frosting()
#' p2 <- predict(wf2, latest)
#' p2
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

#' @importFrom rlang caller_env
validate_frosting <- function(x, ..., arg = "`x`", call = caller_env()) {
  rlang::check_dots_empty()
  if (!is_frosting(x)) {
    glubort(
      "{arg} must be a frosting postprocessor, not a {class(x)[[1]]}.",
      .call = call
    )
  }
  invisible(x)
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
#'
#' @examples
#' library(dplyr)
#' library(recipes)
#'
#' # Toy example to show that frosting can be created and added for postprocessing
#'  f <- frosting()
#'  wf <- epi_workflow() %>% add_frosting(f)
#'
#' # A more realistic example
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_naomit(all_predictors()) %>%
#'   step_naomit(all_outcomes(), skip = TRUE)
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% parsnip::fit(jhu)
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit(.pred)
#'
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#' p
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
apply_frosting.epi_workflow <-
  function(workflow, components, the_fit, the_recipe, ...) {

    if (!has_postprocessor(workflow)) {
      components$predictions <- predict(
        the_fit, components$forged$predictors, ...)
      components$predictions <- dplyr::bind_cols(
        components$keys, components$predictions)
      return(components)
    }

    if (!has_postprocessor_frosting(workflow)) {
      rlang::warn(c("Only postprocessors of class frosting are allowed.",
                    "Returning unpostprocessed predictions."))
      components$predictions <- predict(
        the_fit, components$forged$predictors, ...)
      components$predictions <- dplyr::bind_cols(
        components$keys, components$predictions)
      return(components)
    }

    layers <- extract_layers(workflow)

    # checks if layer_predict() is in the postprocessor
    layer_names <- map_chr(layers, ~ class(.x)[1])
    if (!detect_layer(workflow, "layer_predict")) {
      layers <- c(
        layer_predict_new(NULL, list(), list(), rand_id("predict_default")),
        layers)
    }

    for (l in seq_along(layers)) {
      la <- layers[[l]]
      components <- slather(la, components, the_fit, the_recipe)
    }

    return(components)
  }


layer <- function(subclass, ..., .prefix = "layer_") {
  structure(list(...), class = c(paste0(.prefix, subclass), "layer"))
}

is_layer <- function(x) {
  inherits(x, "layer")
}

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

#' Add layer to a frosting object
#'
#' @param frosting a `frosting` postprocessor
#' @param object a `frosting` layer
#' @param flag logical to determine if the layer is added. Default `TRUE`.
#'
#' @return an updated `frosting` postprocessor
#' @export
add_layer <- function(frosting, object, flag = TRUE) {
  validate_frosting(frosting)
  validate_layer(object)

  if (flag) frosting$layers[[length(frosting$layers) + 1]] <- object

  frosting
}

detect_layer <- function(x, name, ...) {
  UseMethod(x)
}

detect_layer.frosting <- function(x, name, ...) {
  name %in% map_chr(x$layers, ~ class(.x)[1])
}

detect_layer.workflow <- function(x, name, ...) {
  validate_has_postprocessor(x)
  detect_layer(x$post$actions$frosting$frosting)
}

extract_layers <- function(x, ...) {
  UseMethod("extract_layers")
}

extract_layers.frosting <- function(x, ...) {
  rlang::check_dots_empty()
  x$layers
}

extract_layers.workflow <- function(x, ...) {
  rlang::check_dots_empty()
  validate_has_postprocessor(x)
  extract_layers(x$post$actions$frosting$frosting)
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
