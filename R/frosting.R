#' Add frosting to a workflow
#'
#' @param x A workflow
#' @param frosting A frosting layer created using `frosting()`.
#' Optional for `update_epi_recipe()` only.
#' @param layer_num the number of the layer to update in an `epi_workflow`.
#' Intended for use in `update_frosting()` only.
#' @param ... Can only be used in `update_frosting()` to input a parameter
#' update.
#'
#' @return `x`, updated with a new or removed frosting postprocessor
#' @export
#'
#' @details The `update_frosting` function can either update the entire frosting
#' or a layer in an existing frosting in an `epi_workflow`. In the latter case,
#' the parameter name the new value it is equal to must be input into `...`.
#' See the examples below for brief illustrations of both types of updates.
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' latest <- jhu %>%
#'   dplyr::filter(time_value >= max(time_value) - 14)
#'
#' # Add frosting to a workflow and predict
#' f <- frosting() %>% layer_predict() %>% layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#' p1 <- predict(wf1, latest)
#' p1
#'
#' # Update frosting in a workflow and predict
#' f2 <- frosting() %>% layer_predict()
#' wf2 <- wf1 %>% update_frosting(f2)
#' p2 <- predict(wf2, latest)
#' p2
#'
#' # Remove frosting from the workflow and predict
#' wf3 <- wf2 %>% remove_frosting()
#' p2 <- predict(wf3, latest)
#' p2
#'
#' # Additional feature in `update_frosting` is to change a layer
#' # in the frosting from the workflow
#' f3 <- frosting() %>% layer_predict() %>% layer_threshold(.pred)
#'
#' wf3 = wf %>% add_frosting(f3)
#'
#' # Update `layer_threshold` to have an upper bound of 1
#' wf3 = wf3 %>% update_frosting(layer_num = 2, upper = 1)
#' extract_frosting(wf3)
#'
add_frosting <- function(x, frosting, ...) {
  rlang::check_dots_empty()
  action <- workflows:::new_action_post(frosting = frosting)
  epi_add_action(x, action, "frosting")
}


# Hacks around workflows `order_stage_post <- charcter(0)` ----------------
epi_add_action <- function(x, action, name, ..., call = caller_env()) {
  workflows:::validate_is_workflow(x, call = call)
  add_action_frosting(x, action, name, ..., call = call)
}
add_action_frosting <- function(x, action, name, ..., call = caller_env()) {
  workflows:::check_singleton(x$post$actions, name, call = call)
  x$post <- workflows:::add_action_to_stage(x$post, action, name, order_stage_frosting())
  x
}
order_stage_frosting <- function() "frosting"
# End hacks. See cmu-delphi/epipredict#75


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

#' @rdname add_frosting
#' @export
update_frosting <- function(x, frosting = NULL, layer_num = NULL, ...) {

  if (is_null(frosting) && !is_null(layer_num)) {
    frosting <- extract_frosting(x)
    frosting$layers[[layer_num]] <- update(frosting$layers[[layer_num]], ...)
  }

  x <- remove_frosting(x)
  add_frosting(x, frosting)
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
#'
#' # Toy example to show that frosting can be created and added for postprocessing
#'  f <- frosting()
#'  wf <- epi_workflow() %>% add_frosting(f)
#'
#' # A more realistic example
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
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


#' Extract the frosting object from a workflow
#'
#' @param x an `epi_workflow` object
#' @param ... not used
#'
#' @return a `frosting` object
#' @export
extract_frosting <- function(x, ...) {
  UseMethod("extract_frosting")
}

#' @export
extract_frosting.default <- function(x, ...) {
  abort(c("Frosting is only available for epi_workflows currently.",
          i = "Can you use `epi_workflow()` instead of `workflow()`?"))
  invisible(x)
}

#' @export
extract_frosting.epi_workflow <- function(x, ...) {
  if (has_postprocessor_frosting(x)) return(x$post$actions$frosting$frosting)
  else cli_stop("The epi_workflow does not have a postprocessor.")
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
  function(workflow, components, new_data, ...) {

    the_fit <- workflows::extract_fit_parsnip(workflow)

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

    # Check if there's a predict layer, add it if not.
    if (rlang::is_null(layers)) {
      layers <- extract_layers(frosting() %>% layer_predict())
    } else if (!detect_layer(workflow, "layer_predict")) {
      layers <- c(list(
        layer_predict_new(NULL, list(), list(), rand_id("predict_default"))),
        layers)
    }

    for (l in seq_along(layers)) {
      la <- layers[[l]]
      components <- slather(la, components, workflow, new_data)
    }

    return(components)
  }

#' @export
print.frosting <- function(x, form_width = 30, ...) {
  cli::cli_div(
    theme = list(.pkg = list(`vec-trunc` = Inf, `vec-last` = ", "))
  )
  cli::cli_h1("Frosting")

  if (!is.null(x$layers)) cli::cli_h3("Layers")
  for (layer in x$layers) print(layer, form_width = form_width)
  cli::cli_end()
  invisible(x)
}

# Currently only used in the workflow printing
print_frosting <- function(x, ...) {

  layers <- x$layers
  n_layers <- length(layers)
  layer <- ifelse(n_layers == 1L, "Layer", "Layers")
  n_layers_msg <- glue::glue("{n_layers} Frosting {layer}")
  cat_line(n_layers_msg)

  if (n_layers == 0L) return(invisible(x))

  cat_line("")

  layer_names <- map_chr(layers, pull_layer_name)

  if (n_layers <= 10L) {
    cli::cat_bullet(layer_names)
    return(invisible(x))
  }

  extra_layers <- n_layers - 10L
  layer_names <- layer_names[1:10]

  layer <- ifelse(extra_layers == 1L, "layer", "layers")

  extra_dots <- "..."
  extra_msg <- glue::glue("and {extra_layers} more {layer}.")

  layer_names <- c(layer_names, extra_dots, extra_msg)

  cli::cat_bullet(layer_names)
  invisible(x)
}

print_postprocessor <- function(x) {
  if (!has_postprocessor_frosting(x)) return(invisible(x))

  header <- cli::rule("Postprocessor")
  cat_line(header)

  frost <- extract_frosting(x)
  print_frosting(frost)

  invisible(x)
}
