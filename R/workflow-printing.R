print_header <- function(x) {
  cli::cli_text("")
  trained <- ifelse(workflows::is_trained_workflow(x), " [trained]", "")
  d <- cli::cli_div(theme = list(rule = list("line-type" = "double")))
  cli::cli_rule("Epi Workflow{trained}")
  cli::cli_end(d)

  preprocessor_msg <- cli::style_italic("Preprocessor:")
  preprocessor <- dplyr::case_when(
    workflows:::has_preprocessor_formula(x) ~ "Formula",
    workflows:::has_preprocessor_recipe(x) ~ "Recipe",
    workflows:::has_preprocessor_variables(x) ~ "Variables",
    TRUE ~ "None"
  )
  cli::cli_text("{.emph Preprocessor:} {preprocessor}")


  if (workflows:::has_spec(x)) {
    spec <- class(workflows::extract_spec_parsnip(x))[[1]]
    spec <- glue::glue("{spec}()")
  } else {
    spec <- "None"
  }
  cli::cli_text("{.emph Model:} {spec}")

  postprocessor <- ifelse(has_postprocessor_frosting(x), "Frosting", "None")
  cli::cli_text("{.emph Postprocessor:} {postprocessor}")
  cli::cli_text("")
  invisible(x)
}


print_preprocessor <- function(x) {
  has_preprocessor_formula <- workflows:::has_preprocessor_formula(x)
  has_preprocessor_recipe <- workflows:::has_preprocessor_recipe(x)
  has_preprocessor_variables <- workflows:::has_preprocessor_variables(x)

  no_preprocessor <- !has_preprocessor_formula && !has_preprocessor_recipe &&
    !has_preprocessor_variables

  if (no_preprocessor) {
    return(invisible(x))
  }

  cli::cli_rule("Preprocessor")
  cli::cli_text("")

  if (has_preprocessor_formula) {
    print_preprocessor_formula(x)
  }
  if (has_preprocessor_recipe) {
    print_preprocessor_recipe(x)
  }
  if (has_preprocessor_variables) {
    print_preprocessor_variables(x)
  }
  cli::cli_text("")
  invisible(x)
}

# revision of workflows:::print_model()
print_model <- function(x) {
  has_spec <- workflows:::has_spec(x)
  if (!has_spec) {
    cli::cli_text("")
    return(invisible(x))
  }
  has_fit <- workflows:::has_fit(x)
  cli::cli_rule("Model")

  if (has_fit) {
    print_fit(x)
    cli::cli_text("")
    return(invisible(x))
  }
  workflows:::print_spec(x)
  cli::cli_text("")
  invisible(x)
}


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


# subfunctions for printing -----------------------------------------------



print_preprocessor_formula <- function(x) {
  formula <- workflows::extract_preprocessor(x)
  formula <- rlang::expr_text(formula)
  cli::cli_text(formula)
  invisible(x)
}

print_prepocessor_variables <- function(x) {
  variables <- workflows::extract_preprocessor(x)
  outcomes <- rlang::quo_get_expr(variables$outcomes)
  predictors <- rlang::quo_get_expr(variables$predictors)
  outcomes <- rlang::expr_text(outcomes)
  predictors <- rlang::expr_text(predictors)
  cli::cli_text("Outcomes: ", outcomes)
  cli::cli_text("")
  cli::cli_text("Predictors: ", predictors)
  invisible(x)
}

# Currently only used in the workflow printing
print_preprocessor_recipe <- function(x, ...) {
  recipe <- workflows::extract_preprocessor(x)
  steps <- recipe$steps
  n_steps <- length(steps)
  cli::cli_text("{n_steps} Recipe step{?s}.")

  if (n_steps == 0L) {
    return(invisible(x))
  }

  step_names <- map_chr(steps, workflows:::pull_step_name)

  if (n_steps <= 10L) {
    cli::cli_ol(step_names)
    return(invisible(x))
  }

  extra_steps <- n_steps - 10L
  step_names <- step_names[1:10]

  cli::cli_ol(step_names)
  cli::cli_bullets("... and {extra_steps} more step{?s}.")
  invisible(x)
}




print_fit <- function(x) {
  parsnip_fit <- workflows::extract_fit_parsnip(x)
  fit <- parsnip_fit$fit
  output <- utils::capture.output(fit)
  n_output <- length(output)
  if (n_output < 50L) {
    print(fit)
    return(invisible(x))
  }
  n_extra_output <- n_output - 50L
  output <- output[1:50]
  empty_string <- output == ""
  output[empty_string] <- " "

  cli::cli_verbatim(output)
  cli::cli_text("")
  cli::cli_text("... and {n_extra_output} more line{?s}.")
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
