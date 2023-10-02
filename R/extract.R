#' Extract an argument made to a frosting layer or recipe step
#'
#' @param x an epi_workflow, epi_recipe, frosting, step, or layer object
#' @param name the name of the layer
#' @param arg the name of the argument
#' @param ... not used
#'
#' @return An object originally passed as an argument to a layer or step
#' @export
#'
#' @keywords internal
#'
#' @examples
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles(quantile_levels = c(0.0275, 0.975), symmetrize = FALSE) %>%
#'   layer_naomit(.pred)
#'
#' extract_argument(f, "layer_residual_quantiles", "symmetrize")
extract_argument <- function(x, name, arg, ...) {
  UseMethod("extract_argument")
}

#' @export
extract_argument.layer <- function(x, name, arg, ...) {
  rlang::check_dots_empty()
  arg_is_chr_scalar(name, arg)
  in_layer_name <- class(x)[1]
  if (name != in_layer_name) {
    cli_abort("Requested {.val {name}} not found. This is a(n) {.cls {in_layer_name}}.")
  }
  if (!arg %in% names(x)) {
    cli_abort("Requested argument {.val {arg}} not found in {.val {name}}.")
  }
  x[[arg]]
}

#' @export
extract_argument.step <- function(x, name, arg, ...) {
  rlang::check_dots_empty()
  arg_is_chr_scalar(name, arg)
  in_step_name <- class(x)[1]
  if (name != in_step_name) {
    cli_abort("Requested {.val {name}} not found. This is a {.cls {in_step_name}}.")
  }
  if (!arg %in% names(x)) {
    cli_abort("Requested argument {.val {arg}} not found in {.val {name}}.")
  }
  x[[arg]]
}

#' @export
extract_argument.recipe <- function(x, name, arg, ...) {
  rlang::check_dots_empty()
  step_names <- map_chr(x$steps, ~ class(.x)[1])
  has_step <- name %in% step_names
  if (!has_step) {
    cli_abort("recipe object does not contain a {.val {name}}.")
  }
  step_locations <- which(name == step_names)
  out <- map(x$steps[step_locations], extract_argument, name = name, arg = arg)
  if (length(out) == 1) out <- out[[1]]
  out
}

#' @export
extract_argument.frosting <- function(x, name, arg, ...) {
  rlang::check_dots_empty()
  layer_names <- map_chr(x$layers, ~ class(.x)[1])
  has_layer <- name %in% layer_names
  if (!has_layer) {
    cli_abort("frosting object does not contain a {.val {name}}.")
  }
  layer_locations <- which(name == layer_names)
  out <- map(x$layers[layer_locations], extract_argument, name = name, arg = arg)
  if (length(out) == 1) out <- out[[1]]
  out
}

#' @export
extract_argument.epi_workflow <- function(x, name, arg, ...) {
  rlang::check_dots_empty()
  type <- sub("_.*", "", name)
  if (type %in% c("check", "step")) {
    if (!workflows:::has_preprocessor_recipe(x)) {
      cli_abort("The workflow must have a recipe preprocessor.")
    }
    out <- extract_argument(x$pre$actions$recipe$recipe, name, arg)
  }
  if (type %in% "layer") {
    out <- extract_argument(extract_frosting(x), name, arg)
  }
  if (!type %in% c("check", "step", "layer")) {
    cli_abort("{.val {name}} must begin with one of step, check, or layer")
  }
  return(out)
}


#' @export
#' @rdname layer-processors
extract_layers <- function(x, ...) {
  UseMethod("extract_layers")
}


#' @export
#' @rdname layer-processors
extract_layers.frosting <- function(x, ...) {
  rlang::check_dots_empty()
  x$layers
}

#' @export
#' @rdname layer-processors
extract_layers.workflow <- function(x, ...) {
  rlang::check_dots_empty()
  validate_has_postprocessor(x)
  extract_layers(x$post$actions$frosting$frosting)
}
