#' Convert per-capita predictions to raw scale
#'
#' `layer_population_scaling` creates a specification of a frosting layer
#' that will "undo" per-capita scaling. Typical usage would
#' load a dataset that contains state-level population, and use it to convert
#' predictions made from a rate-scale model to raw scale by multiplying by
#' the population.
#' Although, it is worth noting that there is nothing special about "population".
#' The function can be used to scale by any variable. Population is the
#' standard use case in the epidemiology forecasting scenario. Any value
#' passed will *multiply* the selected variables while the `rate_rescaling`
#' argument is a common *divisor* of the selected variables.
#'
#' @param frosting a `frosting` postprocessor. The layer will be added to the
#'   sequence of operations for this frosting.
#' @param ... One or more selector functions to scale variables
#'   for this step. See [recipes::selections()] for more details.
#' @param df a data frame that contains the population data to be used for
#'   inverting the existing scaling.
#' @param by A (possibly named) character vector of variables to join by.
#'
#' If `NULL`, the default, the function will perform a natural join, using all
#' variables in common across the `epi_df` produced by the `predict()` call
#' and the user-provided dataset.
#' If columns in that `epi_df` and `df` have the same name (and aren't
#' included in `by`), `.df` is added to the one from the user-provided data
#' to disambiguate.
#'
#' To join by different variables on the `epi_df` and `df`, use a named vector.
#' For example, `by = c("geo_value" = "states")` will match `epi_df$geo_value`
#' to `df$states`. To join by multiple variables, use a vector with length > 1.
#' For example, `by = c("geo_value" = "states", "county" = "county")` will match
#' `epi_df$geo_value` to `df$states` and `epi_df$county` to `df$county`.
#'
#' See [dplyr::left_join()] for more details.
#' @param df_pop_col the name of the column in the data frame `df` that
#' contains the population data and used for scaling.
#' @param rate_rescaling Sometimes rates are "per 100K" or "per 1M" rather than
#'   "per person". Adjustments can be made here. For example, if the original
#'   rate is "per 100K", then set `rate_rescaling = 1e5` to get counts back.
#' @param create_new TRUE to create a new column and keep the original column
#' in the `epi_df`.
#' @param suffix a character. The suffix added to the column name if
#' `create_new = TRUE`. Default to "_scaled".
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' library(dplyr)
#' jhu <- epidatasets::cases_deaths_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
#'   select(geo_value, time_value, cases)
#'
#' pop_data <- data.frame(states = c("ca", "ny"), value = c(20000, 30000))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_population_scaling(
#'     df = pop_data,
#'     df_pop_col = "value",
#'     by = c("geo_value" = "states"),
#'     cases, suffix = "_scaled"
#'   ) %>%
#'   step_epi_lag(cases_scaled, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(cases_scaled, ahead = 7, role = "outcome") %>%
#'   step_epi_naomit()
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_threshold(.pred) %>%
#'   layer_naomit(.pred) %>%
#'   layer_population_scaling(.pred,
#'     df = pop_data,
#'     by = c("geo_value" = "states"),
#'     df_pop_col = "value"
#'   )
#'
#' wf <- epi_workflow(r, linear_reg()) %>%
#'   fit(jhu) %>%
#'   add_frosting(f)
#'
#' forecast(wf)
layer_population_scaling <- function(frosting,
                                     ...,
                                     df,
                                     by = NULL,
                                     df_pop_col,
                                     rate_rescaling = 1,
                                     create_new = TRUE,
                                     suffix = "_scaled",
                                     id = rand_id("population_scaling")) {
  arg_is_scalar(df_pop_col, rate_rescaling, create_new, suffix, id)
  arg_is_lgl(create_new)
  arg_is_chr(df_pop_col, suffix, id)
  arg_is_chr(by, allow_null = TRUE)
  if (rate_rescaling <= 0) {
    cli_abort("`rate_rescaling` must be a positive number.")
  }

  add_layer(
    frosting,
    layer_population_scaling_new(
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      rate_rescaling = rate_rescaling,
      terms = dplyr::enquos(...),
      create_new = create_new,
      suffix = suffix,
      id = id
    )
  )
}

layer_population_scaling_new <-
  function(df, by, df_pop_col, rate_rescaling, terms, create_new, suffix, id) {
    layer("population_scaling",
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      rate_rescaling = rate_rescaling,
      terms = terms,
      create_new = create_new,
      suffix = suffix,
      id = id
    )
  }

#' @export
slather.layer_population_scaling <-
  function(object, components, workflow, new_data, ...) {
    stopifnot(
      "Only one population column allowed for scaling" =
        length(object$df_pop_col) == 1
    )
    rlang::check_dots_empty()

    if (is.null(object$by)) {
      # Assume `layer_predict` has calculated the prediction keys and other
      # layers don't change the prediction key colnames:
      prediction_key_colnames <- names(components$keys)
      lhs_potential_keys <- prediction_key_colnames
      rhs_potential_keys <- colnames(select(object$df, !object$df_pop_col))
      object$by <- intersect(lhs_potential_keys, rhs_potential_keys)
      suggested_min_keys <- kill_time_value(lhs_potential_keys)
      if (!all(suggested_min_keys %in% object$by)) {
        cli_warn(c(
          "{setdiff(suggested_min_keys, object$by)} {?was an/were} epikey column{?s} in the predictions,
           but {?wasn't/weren't} found in the population `df`.",
          "i" = "Defaulting to join by {object$by}",
          ">" = "Double-check whether column names on the population `df` match those expected in your predictions",
          ">" = "Consider using population data with breakdowns by {suggested_min_keys}",
          ">" = "Manually specify `by =` to silence"
        ), class = "epipredict__layer_population_scaling__default_by_missing_suggested_keys")
      }
    }

    object$by <- object$by %||% intersect(
      epi_keys_only(components$predictions),
      colnames(select(object$df, !object$df_pop_col))
    )
    joinby <- list(x = names(object$by) %||% object$by, y = object$by)
    hardhat::validate_column_names(components$predictions, joinby$x)
    hardhat::validate_column_names(object$df, joinby$y)

    # object$df <- object$df %>%
    #  dplyr::mutate(dplyr::across(tidyselect::where(is.character), tolower))
    pop_col <- rlang::sym(object$df_pop_col)
    exprs <- rlang::expr(c(!!!object$terms))
    pos <- tidyselect::eval_select(exprs, components$predictions)
    col_names <- names(pos)
    suffix <- ifelse(object$create_new, object$suffix, "")
    col_to_remove <- setdiff(colnames(object$df), colnames(components$predictions))

    components$predictions <- inner_join(
      components$predictions,
      object$df,
      by = object$by,
      relationship = "many-to-one",
      unmatched = c("error", "drop"),
      suffix = c("", ".df")
    ) %>%
      mutate(across(
        all_of(col_names),
        ~ .x * !!pop_col / object$rate_rescaling,
        .names = "{.col}{suffix}"
      )) %>%
      select(-any_of(col_to_remove))
    components
  }

#' @export
print.layer_population_scaling <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Scaling predictions by population"
  print_layer(x$terms, title = title, width = width)
}
