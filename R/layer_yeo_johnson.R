#' Unormalizing transformation
#'
#' Will undo a step_epi_YeoJohnson transformation. For practical reasons, if you
#' are using this step on a column that will eventually become the outcome
#' variable, you should make sure that the original name of that column is a
#' subset of the outcome variable name. `ahead_7_cases` when `cases` is
#' transformed will work well, while `ahead_7` will not.
#'
#' @inheritParams layer_population_scaling
#' @param yj_params A data frame of parameters to be used for inverting the
#'   transformation. Typically set automatically. If you have done multiple
#'   transformations such that the outcome variable name no longer contains the
#'   column that this step transforms, then you should manually specify this to
#'   be the parameters fit in the corresponding `step_epi_YeoJohnson`. For an
#'   example where you wouldn't need to set this, if your output is
#'   `ahead_7_cases` and `step_epi_YeoJohnson` transformed cases (possibly with
#'   other columns), then you wouldn't need to set this. However if you have
#'   renamed your output column to `diff_7`, then you will need to extract the `yj_params` from the step.
#' @param by A (possibly named) character vector of variables to join by.
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' library(dplyr)
#' jhu <- epidatasets::cases_deaths_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
#'   select(geo_value, time_value, cases)
#'
#' # Create a recipe with a Yeo-Johnson transformation.
#' r <- epi_recipe(jhu) %>%
#'   step_epi_YeoJohnson(cases) %>%
#'   step_epi_lag(cases, lag = 0) %>%
#'   step_epi_ahead(cases, ahead = 0, role = "outcome") %>%
#'   step_epi_naomit()
#'
#' # Create a frosting layer that will undo the Yeo-Johnson transformation.
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_epi_YeoJohnson(.pred)
#'
#' # Create a workflow and fit it.
#' wf <- epi_workflow(r, linear_reg()) %>%
#'   fit(jhu) %>%
#'   add_frosting(f)
#'
#' # Forecast the workflow, which should reverse the Yeo-Johnson transformation.
#' forecast(wf)
#' # Compare to the original data.
#' jhu %>% filter(time_value == "2021-12-31")
#' forecast(wf)
layer_epi_YeoJohnson <- function(frosting, ..., yj_params = NULL, by = NULL, id = rand_id("epi_YeoJohnson")) {
  checkmate::assert_tibble(yj_params, min.rows = 1, null.ok = TRUE)

  add_layer(
    frosting,
    layer_epi_YeoJohnson_new(
      yj_params = yj_params,
      by = by,
      terms = dplyr::enquos(...),
      id = id
    )
  )
}

layer_epi_YeoJohnson_new <- function(yj_params, by, terms, id) {
  layer("epi_YeoJohnson", yj_params = yj_params, by = by, terms = terms, id = id)
}

#' @export
#' @importFrom workflows extract_preprocessor
slather.layer_epi_YeoJohnson <- function(object, components, workflow, new_data, ...) {
  rlang::check_dots_empty()

  # get the yj_params from the layer or from the workflow.
  yj_params <-
    object$yj_params %||%
    get_params_in_layer(workflow, "epi_YeoJohnson", "yj_params")

  # if the by is not specified, try to infer it from the yj_params.
  if (is.null(object$by)) {
    # if not specified, match the keys used in the join step
    object$by <- key_colnames(new_data, exclude = "time_value")
    yj_params
    # assume `layer_predict` has calculated the prediction keys and other
    # layers don't change the prediction key colnames:
    prediction_key_colnames <- names(components$keys)
    lhs_potential_keys <- prediction_key_colnames
    rhs_potential_keys <- colnames(select(yj_params, -starts_with(".yj_param_")))
    object$by <- intersect(lhs_potential_keys, rhs_potential_keys)
    suggested_min_keys <- setdiff(lhs_potential_keys, "time_value")
    if (!all(suggested_min_keys %in% object$by)) {
      cli_warn(
        c(
          "{setdiff(suggested_min_keys, object$by)} {?was an/were} epikey column{?s} in the predictions,
          but {?wasn't/weren't} found in the population `df`.",
          "i" = "defaulting to join by {object$by}",
          ">" = "double-check whether column names on the population `df` match those expected in your predictions",
          ">" = "consider using population data with breakdowns by {suggested_min_keys}",
          ">" = "manually specify `by =` to silence"
        ),
        class = "epipredict__layer_population_scaling__default_by_missing_suggested_keys"
      )
    }
  }

  # Establish the join columns.
  object$by <- object$by %||%
    intersect(
      epi_keys_only(components$predictions),
      colnames(select(yj_params, -starts_with(".yj_param_")))
    )
  joinby <- list(x = names(object$by) %||% object$by, y = object$by)
  hardhat::validate_column_names(components$predictions, joinby$x)
  hardhat::validate_column_names(yj_params, joinby$y)

  # Join the yj_params.
  components$predictions <- inner_join(
    components$predictions,
    yj_params,
    by = object$by,
    relationship = "many-to-one",
    unmatched = c("error", "drop")
  )

  exprs <- rlang::expr(c(!!!object$terms))
  pos <- tidyselect::eval_select(exprs, components$predictions)
  col_names <- names(pos)

  # The `object$terms` is where the user specifies the columns they want to
  # untransform. We need to match the outcomes with their yj_param columns in our
  # parameter table and then apply the inverse transformation.
  if (length(col_names) == 0) {
    # not specified by the user, so just modify everything starting with `.pred`
    components$predictions <- components$predictions %>%
      mutate(across(
        starts_with(".pred"),
        \(.pred) yj_inverse(.pred, .lambda) # debug(yj_inverse)
      )) %>%
      select(-.lambda)
  } else {
    components$predictions <- components$predictions %>%
      mutate(across(
        all_of(col_names),
        \(.pred) yj_inverse(.pred, .lambda)
      )) %>%
      select(-.lambda)
  }

  # Remove the yj_param columns.
  components$predictions <- components$predictions %>%
    select(-any_of(starts_with(".yj_param_"))) %>%
    ungroup()
  components
}

#' @export
print.layer_epi_YeoJohnson <- function(x, width = max(20, options()$width - 30), ...) {
  title <- "Yeo-Johnson transformation (see `yj_params` object for values) on "
  print_layer(x$terms, title = title, width = width)
}

# Inverse Yeo-Johnson transformation
#
# Inverse of `yj_transform` in step_yeo_johnson.R.
yj_inverse <- function(x_in, lambda, eps = 0.001) {
  if (any(is.na(lambda))) {
    return(x)
  }
  x_lambda <- yj_input_type_management(x_in, lambda)
  x <- x_lambda[[1]]
  lambda <- x_lambda[[2]]
  inv_x <- ifelse(
    x < 0,
    # negative values we test if lambda is ~2
    ifelse(
      abs(lambda - 2) < eps,
      -(exp(-x) - 1),
      -(((lambda - 2) * x + 1)^(1 / (2 - lambda)) - 1)
    ),
    # non-negative values we test if lambda is ~0
    ifelse(
      abs(lambda) < eps,
      (exp(x) - 1),
      (lambda * x + 1)^(1 / lambda) - 1
    )
  )
  if (x_in %>% inherits("quantile_pred")) {
    inv_x <- inv_x %>% quantile_pred(x_in %@% "quantile_levels")
  }
  inv_x
}

yj_inverse.quantile_pred <- function(x, lambda, eps = 0.001) {
}

nn_inv_trans <- function(x, lambda, eps) {
  out <- double(length(x))
  sm_lambdas <- abs(lambda) < eps
  if (length(sm_lambdas) > 0) {
    out[sm_lambdas] <- exp(x[sm_lambdas]) - 1
  }
  x <- x[!sm_lambdas]
  lambda <- lambda[!sm_lambdas]
  if (length(x) > 0) {
    out[!sm_lambdas] <- (lambda * x + 1)^(1 / lambda) - 1
  }
  out
}
nn_inv_trans_new <- function(x, lambda, eps) {
  out <- double(length(x))
  sm_lambdas <- abs(lambda) < eps
  pos_inv_lambda_0 <- fifelse(
    sm_lambdas,
    (exp(x) - 1),
    (lambda * x + 1)^(1 / lambda) - 1
  )
  pos_inv <- (!sm_lambdas) * ((lambda * x + 1)^(1 / lambda) - 1)
  pos_inv_lambda_0
  if (length(sm_lambdas) > 0) {
    out[sm_lambdas] <- exp(x[sm_lambdas]) - 1
  }
  x <- x[!sm_lambdas]
  lambda <- lambda[!sm_lambdas]
  if (length(x) > 0) {
    out[!sm_lambdas] <- (lambda * x + 1)^(1 / lambda) - 1
  }
  out
}

ng_inv_trans <- function(x, lambda, eps) {
  out <- double(length(x))
  near2_lambdas <- abs(lambda - 2) < eps
  if (length(near2_lambdas) > 0) {
    out[near2_lambdas] <- -(exp(-x[near2_lambdas]) - 1)
  }
  x <- x[!near2_lambdas]
  lambda <- lambda[!near2_lambdas]
  if (length(x) > 0) {
    out[!near2_lambdas] <- -(((lambda - 2) * x + 1)^(1 / (2 - lambda)) - 1)
  }
  out
}
ng_inv_trans <- function(x, lambda, eps) {
  out <- double(length(x))
  near2_lambdas <- abs(lambda - 2) < eps
  near2_value <- near2_lambdas * -(exp(-x) - 1)
  away2_value <- -(!near2_lambdas) * (((lambda - 2) * x + 0 * im + 1)^(1 / (2 - lambda)) - 1)
  0 * (near2_value + away2_value)
  x <- x[!near2_lambdas]
  lambda <- lambda[!near2_lambdas]
  if (length(x) > 0) {
    out[!near2_lambdas] <- -(((lambda - 2) * x + 1)^(1 / (2 - lambda)) - 1)
  }
  out
}
#' get the parameters used in the initial step
#'
#' @param workflow the workflow to extract the parameters from
#' @param step_name the name of the step to look for, as recognized by `detect_step`
#' @param param_name the parameter to pull out of the step
#' @keywords internal
get_params_in_layer <- function(workflow, step_name = "epi_YeoJohnson", param_name = "yj_params") {
  full_step_name <- glue::glue("step_{step_name}")
  this_recipe <- hardhat::extract_recipe(workflow)
  if (!(this_recipe %>% recipes::detect_step(step_name))) {
    cli_abort("`layer_{step_name}` requires `step_{step_name}` in the recipe.", call = rlang::caller_env())
  }
  outcomes <-
    workflows::extract_recipe(workflow)$term_info %>%
    filter(role == "outcome") %>%
    pull(variable)
  if (length(outcomes) > 1) {
    cli_abort(
      "`layer_{step_name}` doesn't support multiple output columns. This workflow produces {outcomes} as output columns.",
      call = rlang::caller_env(),
      class = "epipredict__layer_yeo_johnson_multi_outcome_error"
    )
  }
  for (step in this_recipe$steps) {
    # if it's a `step_name` step that also transforms a column that is a subset
    # of the output column name
    is_outcome_subset <- map_lgl(step$columns, ~ grepl(.x, outcomes))
    if (inherits(step, full_step_name) &&
      any(is_outcome_subset)
    ) {
      params <- step[[param_name]] %>%
        select(
          key_colnames(workflow$original_data, exclude = "time_value"),
          contains(step$columns[is_outcome_subset])
        ) %>%
        rename(.lambda = contains(step$columns))
      break
    }
  }
  params
}
