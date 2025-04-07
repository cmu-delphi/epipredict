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
layer_epi_YeoJohnson <- function(frosting, ..., yj_params = NULL, id = rand_id("epi_YeoJohnson")) {
  checkmate::assert_tibble(yj_params, min.rows = 1, null.ok = TRUE)

  add_layer(
    frosting,
    layer_epi_YeoJohnson_new(
      yj_params = yj_params,
      terms = dplyr::enquos(...),
      id = id
    )
  )
}

layer_epi_YeoJohnson_new <- function(yj_params, terms, id) {
  layer("epi_YeoJohnson", yj_params = yj_params, terms = terms, id = id)
}

#' @export
#' @importFrom workflows extract_preprocessor
slather.layer_epi_YeoJohnson <- function(object, components, workflow, new_data, ...) {
  rlang::check_dots_empty()

  # get the yj_params from the layer or from the workflow.
  yj_params <-
    object$yj_params %||%
    get_params_in_layer(workflow, "epi_YeoJohnson", "yj_params")

  # Establish the join columns.
  join_by_columns <- key_colnames(new_data, exclude = "time_value") %>% sort()
  joinby <- list(x = join_by_columns, y = join_by_columns)
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
