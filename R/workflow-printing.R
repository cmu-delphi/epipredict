print_postprocessor <- function(x) {
  if (!has_postprocessor_frosting(x)) {
    return(invisible(x))
  }

  cli::cli_rule("Postprocessor")
  cli::cli_text("")

  frost <- extract_frosting(x)
  print_frosting(frost)
  cli::cli_text("")
  invisible(x)
}


# Currently only used in the workflow printing
print_frosting <- function(x, ...) {
  layers <- x$layers
  n_layers <- length(layers)
  cli::cli_text("{n_layers} Frosting layer{?s}.")

  if (n_layers == 0L) {
    return(invisible(x))
  }

  layer_names <- map_chr(layers, pull_layer_name)

  if (n_layers <= 10L) {
    cli::cli_ol(layer_names)
    return(invisible(x))
  }

  extra_layers <- n_layers - 10L
  layer_names <- layer_names[1:10]

  cli::cli_ol(layer_names)
  cli::cli_bullets("... and {extra_layers} more layer{?s}.")
  invisible(x)
}
