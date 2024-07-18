#' Add frosting to a workflow
#'
#' @param x A workflow
#' @param frosting A frosting object created using `frosting()`.
#' @param ... Not used.
#'
#' @return `x`, updated with a new frosting postprocessor
#' @export
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7)
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' latest <- jhu %>%
#'   dplyr::filter(time_value >= max(time_value) - 14)
#'
#' # Add frosting to a workflow and predict
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit(.pred)
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
#' p3 <- predict(wf3, latest)
#' p3
#'
add_frosting <- function(x, frosting, ...) {
  rlang::check_dots_empty()
  action <- workflows:::new_action_post(frosting = frosting)
  epi_add_action(x, action, "frosting", ...)
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
      i = "Provide one with `add_frosting()`."
    )
    rlang::abort(message, call = call)
  }
  invisible(x)
}

#' @rdname add_frosting
#' @export
update_frosting <- function(x, frosting, ...) {
  rlang::check_dots_empty()
  x <- remove_frosting(x)
  add_frosting(x, frosting)
}


#' Adjust a layer in an `epi_workflow` or `frosting`
#'
#' Make a parameter adjustment to a layer in either an
#' `epi_workflow` or `frosting` object.
#'
#'
#' @details This function can either adjust a layer in a `frosting` object
#' or a layer from a `frosting` object in an `epi_workflow`. The layer to be
#' adjusted is indicated by either the layer number or name (if a name is used,
#' it must be unique). In either case, the argument name and update value
#' must be inputted as `...`. See the examples below for brief
#' illustrations of the different types of updates.
#'
#' @param x An `epi_workflow` or `frosting` object
#'
#' @param which_layer the number or name of the layer to adjust
#'
#' @param ... Used to input a parameter adjustment
#'
#' @return
#' `x`, updated with the adjustment to the specified `frosting` layer.
#'
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#'
#' # in the frosting from the workflow
#' f1 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_threshold(.pred)
#'
#' wf2 <- wf %>% add_frosting(f1)
#'
#' # Adjust `layer_threshold` to have an upper bound of 1
#' # in the `epi_workflow`
#' # Option 1. Using the layer number:
#' wf2 <- wf2 %>% adjust_frosting(which_layer = 2, upper = 1)
#' extract_frosting(wf2)
#' # Option 2. Using the layer name:
#' wf3 <- wf2 %>% adjust_frosting(which_layer = "layer_threshold", upper = 1)
#' extract_frosting(wf3)
#'
#' # Adjust `layer_threshold` to have an upper bound of 5
#' # in the `frosting` object
#' # Option 1. Using the layer number:
#' f2 <- f1 %>% adjust_frosting(which_layer = 2, upper = 5)
#' f2
#' # Option 2. Using the layer name
#' f3 <- f1 %>% adjust_frosting(which_layer = "layer_threshold", upper = 5)
#' f3
#'
adjust_frosting <- function(x, which_layer, ...) {
  UseMethod("adjust_frosting")
}

#' @rdname adjust_frosting
#' @export
adjust_frosting.epi_workflow <- function(
    x, which_layer, ...) {
  frosting <- adjust_frosting(extract_frosting(x), which_layer, ...)

  update_frosting(x, frosting)
}

#' @rdname adjust_frosting
#' @export
adjust_frosting.frosting <- function(
    x, which_layer, ...) {
  if (!(is.numeric(which_layer) || is.character(which_layer))) {
    cli::cli_abort(
      c("`which_layer` must be a number or a character.",
        i = "`which_layer` has class {.cls {class(which_layer)[1]}}."
      )
    )
  } else if (is.numeric(which_layer)) {
    x$layers[[which_layer]] <- update(x$layers[[which_layer]], ...)
  } else {
    layer_names <- map_chr(x$layers, ~ attr(.x, "class")[1])
    starts_with_layer <- substr(which_layer, 1, 6) == "layer_"
    if (!starts_with_layer) which_layer <- paste0("layer_", which_layer)

    if (!(which_layer %in% layer_names)) {
      cli::cli_abort(c(
        "`which_layer` does not appear in the available `frosting` layer names. ",
        i = "The layer names are {.val {layer_names}}."
      ))
    }
    which_layer_idx <- which(layer_names == which_layer)
    if (length(which_layer_idx) == 1) {
      x$layers[[which_layer_idx]] <- update(x$layers[[which_layer_idx]], ...)
    } else {
      cli::cli_abort(c(
        "`which_layer` is not unique. Matches layers: {.val {which_layer_idx}}.",
        i = "Please use the layer number instead for precise alterations."
      ))
    }
  }
  x
}



#' @importFrom rlang caller_env
add_postprocessor <- function(x, postprocessor, ..., call = caller_env()) {
  rlang::check_dots_empty()
  if (is_frosting(postprocessor)) {
    return(add_frosting(x, postprocessor))
  }
  cli::cli_abort("`postprocessor` must be a frosting object.", call = call)
}

is_frosting <- function(x) {
  inherits(x, "frosting")
}

#' @importFrom rlang caller_env
validate_frosting <- function(x, ..., arg = "`x`", call = caller_env()) {
  rlang::check_dots_empty()
  if (!is_frosting(x)) {
    cli::cli_abort(
      "{arg} must be a frosting postprocessor, not a {.cls {class(x)[[1]]}}.",
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
#' f <- frosting()
#' wf <- epi_workflow() %>% add_frosting(f)
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
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit(.pred)
#'
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- forecast(wf1)
#' p
frosting <- function(layers = NULL, requirements = NULL) {
  if (!is_null(layers) || !is_null(requirements)) {
    cli::cli_abort(
      "Currently, no arguments to `frosting()` are allowed to be non-null."
    )
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
  cli::cli_abort(c(
    "Frosting is only available for epi_workflows currently.",
    i = "Can you use `epi_workflow()` instead of `workflow()`?"
  ))
  invisible(x)
}

#' @export
extract_frosting.epi_workflow <- function(x, ...) {
  if (has_postprocessor_frosting(x)) {
    return(x$post$actions$frosting$frosting)
  } else {
    cli_stop("The epi_workflow does not have a postprocessor.")
  }
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
    cli::cli_abort(c(
      "Postprocessing is only available for epi_workflows currently.",
      i = "Can you use `epi_workflow()` instead of `workflow()`?"
    ))
  }
  return(components)
}



#' @rdname apply_frosting
#' @importFrom rlang is_null
#' @importFrom rlang abort
#' @export
apply_frosting.epi_workflow <-
  function(workflow, components, new_data, type = NULL, opts = list(), ...) {
    the_fit <- workflows::extract_fit_parsnip(workflow)

    if (!has_postprocessor(workflow)) {
      components$predictions <- predict(
        the_fit, components$forged$predictors, ...
      )
      components$predictions <- dplyr::bind_cols(
        components$keys, components$predictions
      )
      return(components)
    }

    if (!has_postprocessor_frosting(workflow)) {
      cli::cli_warn(c(
        "Only postprocessors of class {.cls frosting} are allowed.",
        "Returning unpostprocessed predictions."
      ))
      components$predictions <- predict(
        the_fit, components$forged$predictors, ...
      )
      components$predictions <- dplyr::bind_cols(
        components$keys, components$predictions
      )
      return(components)
    }

    layers <- extract_layers(workflow)

    # Check if there's a predict layer, add it if not.
    if (rlang::is_null(layers)) {
      layers <- extract_layers(frosting() %>% layer_predict())
    } else if (!detect_layer(workflow, "layer_predict")) {
      layers <- c(
        list(
          layer_predict_new(NULL, list(), list(), rand_id("predict_default"))
        ),
        layers
      )
    }
    if (length(layers) > 1L &&
          (!is.null(type) || !identical(opts, list()) || rlang::dots_n(...) > 0L)) {
      cli_abort("
        Passing `type`, `opts`, or `...` into `predict.epi_workflow()` is not
        supported if you have frosting layers other than `layer_predict`. Please
        provide these arguments earlier (i.e. while constructing the frosting
        object) by passing them into an explicit call to `layer_predict(), and
        adjust the remaining layers to account for resulting differences in
        output format from these settings.
      ", class = "epipredict__apply_frosting__predict_settings_with_unsupported_layers")
    }

    for (l in seq_along(layers)) {
      la <- layers[[l]]
      if (inherits(la, "layer_predict")) {
        components <- slather(la, components, workflow, new_data, type = type, opts = opts, ...)
      } else {
        # The check above should ensure we have default `type` and `opts` and
        # empty `...`; don't forward these default `type` and `opts`, to avoid
        # upsetting some slather method validation.
        components <- slather(la, components, workflow, new_data)
      }
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

  fmt <- cli::cli_fmt({
    for (layer in x$layers) {
      print(layer, form_width = form_width)
    }
  })
  cli::cli_ol(fmt)
  cli::cli_end()
  invisible(x)
}
