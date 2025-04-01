#' Unormalizing transformation
#'
#' Will undo a step_epi_YeoJohnson transformation.
#'
#' @inheritParams layer_population_scaling
#' @param yj_params Internal. A data frame of parameters to be used for
#'   inverting the transformation.
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

  # TODO: We will error if we don't have a workflow. Write a check later.

  # Get the yj_params from the layer or from the workflow.
  yj_params <- object$yj_params %||% get_yj_params_in_layer(workflow)

  # If the by is not specified, try to infer it from the yj_params.
  if (is.null(object$by)) {
    # Assume `layer_predict` has calculated the prediction keys and other
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
          "i" = "Defaulting to join by {object$by}",
          ">" = "Double-check whether column names on the population `df` match those expected in your predictions",
          ">" = "Consider using population data with breakdowns by {suggested_min_keys}",
          ">" = "Manually specify `by =` to silence"
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
  if (identical(col_names, ".pred")) {
    # In this case, we don't get a hint for the outcome column name, so we need
    # to infer it from the mold.
    if (length(components$mold$outcomes) > 1) {
      cli_abort("Only one outcome is allowed when specifying `.pred`.", call = rlang::caller_env())
    }
    # `outcomes` is a vector of objects like ahead_1_cases, ahead_7_cases, etc.
    # We want to extract the cases part.
    outcome_cols <- names(components$mold$outcomes) %>%
      stringr::str_match("ahead_\\d+_(.*)") %>%
      magrittr::extract(, 2)

    components$predictions <- components$predictions %>%
      mutate(.pred := yj_inverse(.pred, !!sym(paste0(".yj_param_", outcome_cols))))
  } else if (identical(col_names, character(0))) {
    # Wish I could suggest `all_outcomes()` here, but currently it's the same as
    # not specifying any terms. I don't want to spend time with dealing with
    # this case until someone asks for it.
    cli::cli_abort(
      "Not specifying columns to layer Yeo-Johnson is not implemented.
    If you had a single outcome, you can use `.pred` as a column name.
    If you had multiple outcomes, you'll need to specify them like
    `.pred_ahead_1_<outcome_col>`, `.pred_ahead_7_<outcome_col>`, etc.
    ",
      call = rlang::caller_env()
    )
  } else {
    # In this case, we assume that the user has specified the columns they want
    # transformed here. We then need to determine the yj_param columns for each of
    # these columns. That is, we need to convert a vector of column names like
    # c(".pred_ahead_1_case_rate", ".pred_ahead_7_case_rate") to
    # c(".yj_param_ahead_1_case_rate", ".yj_param_ahead_7_case_rate").
    original_outcome_cols <- stringr::str_match(col_names, ".pred_ahead_\\d+_(.*)")[, 2]
    outcomes_wout_ahead <- stringr::str_match(names(components$mold$outcomes), "ahead_\\d+_(.*)")[, 2]
    if (any(original_outcome_cols %nin% outcomes_wout_ahead)) {
      cli_abort(
        "All columns specified in `...` must be outcome columns.
      They must be of the form `.pred_ahead_1_<outcome_col>`, `.pred_ahead_7_<outcome_col>`, etc.
      ",
        call = rlang::caller_env()
      )
    }

    for (i in seq_along(col_names)) {
      col <- col_names[i]
      yj_param_col <- paste0(".yj_param_", original_outcome_cols[i])
      components$predictions <- components$predictions %>%
        mutate(!!sym(col) := yj_inverse(!!sym(col), !!sym(yj_param_col)))
    }
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
yj_inverse <- function(x, lambda, eps = 0.001) {
  if (any(is.na(lambda))) {
    return(x)
  }
  if (length(x) > 1 && length(lambda) == 1) {
    lambda <- rep(lambda, length(x))
  } else if (length(x) != length(lambda)) {
    cli::cli_abort("Length of `x` must be equal to length of `lambda`.", call = rlang::caller_fn())
  }
  if (!inherits(x, "tbl_df") || is.data.frame(x)) {
    x <- unlist(x, use.names = FALSE)
  } else {
    if (!is.vector(x)) {
      x <- as.vector(x)
    }
  }

  nn_inv_trans <- function(x, lambda) {
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

  ng_inv_trans <- function(x, lambda) {
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

  dat_neg <- x < 0
  not_neg <- which(!dat_neg)
  is_neg <- which(dat_neg)

  if (length(not_neg) > 0) {
    x[not_neg] <- nn_inv_trans(x[not_neg], lambda[not_neg])
  }

  if (length(is_neg) > 0) {
    x[is_neg] <- ng_inv_trans(x[is_neg], lambda[is_neg])
  }
  x
}

get_yj_params_in_layer <- function(workflow) {
  this_recipe <- hardhat::extract_recipe(workflow)
  if (!(this_recipe %>% recipes::detect_step("epi_YeoJohnson"))) {
    cli_abort("`layer_epi_YeoJohnson` requires `step_epi_YeoJohnson` in the recipe.", call = rlang::caller_env())
  }
  for (step in this_recipe$steps) {
    if (inherits(step, "step_epi_YeoJohnson")) {
      yj_params <- step$yj_params
      break
    }
  }
  yj_params
}
