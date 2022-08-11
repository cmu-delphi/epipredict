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

    # Check if there's a predict layer, add it if not.
    if (rlang::is_null(layers)) {
      layers <- extract_layers(frosting() %>% layer_predict())
    } else if (! detect_layer(workflow, "layer_predict")) {
      layers <- c(list(
        layer_predict_new(NULL, list(), list(), rand_id("predict_default"))),
        layers)
    }

    for (l in seq_along(layers)) {
      la <- layers[[l]]
      components <- slather(la, components, the_fit, the_recipe)
    }

    return(components)
  }


#' @export
print.frosting <- function(x, ...) {

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

  frosting <- extract_frosting(x)
  print(frosting)

  invisible(x)
}
