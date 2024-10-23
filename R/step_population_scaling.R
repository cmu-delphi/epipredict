#' Convert raw scale predictions to per-capita
#'
#' `step_population_scaling` creates a specification of a recipe step
#' that will perform per-capita scaling. Typical usage would
#' load a dataset that contains state-level population, and use it to convert
#' predictions made from a raw scale model to rate-scale by dividing by
#' the population.
#' Although, it is worth noting that there is nothing special about "population".
#' The function can be used to scale by any variable. Population is the
#' standard use case in the epidemiology forecasting scenario. Any value
#' passed will *divide* the selected variables while the `rate_rescaling`
#' argument is a common *multiplier* of the selected variables.
#'
#' @inheritParams step_epi_lag
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
#' contains the population data and will be used for scaling.
#' This should be one column.
#' @param rate_rescaling Sometimes raw scales are "per 100K" or "per 1M".
#' Adjustments can be made here. For example, if the original
#' scale is "per 100K", then set `rate_rescaling = 1e5` to get rates.
#' @param create_new TRUE to create a new column and keep the original column
#' in the `epi_df`
#' @param suffix a character. The suffix added to the column name if
#' `create_new = TRUE`. Default to "_scaled".
#'
#' @return Scales raw data by the population
#' @export
#' @examples
#' library(dplyr)
#' jhu <- cases_deaths_subset %>%
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
step_population_scaling <-
  function(recipe,
           ...,
           role = "raw",
           df,
           by = NULL,
           df_pop_col,
           rate_rescaling = 1,
           create_new = TRUE,
           suffix = "_scaled",
           skip = FALSE,
           id = rand_id("population_scaling")) {
    arg_is_scalar(role, df_pop_col, rate_rescaling, create_new, suffix, id)
    arg_is_lgl(create_new, skip)
    arg_is_chr(df_pop_col, suffix, id)
    arg_is_chr(by, allow_null = TRUE)
    if (rate_rescaling <= 0) {
      cli_abort("`rate_rescaling` must be a positive number.")
    }

    recipes::add_step(
      recipe,
      step_population_scaling_new(
        terms = enquos(...),
        role = role,
        trained = FALSE,
        df = df,
        by = by,
        df_pop_col = df_pop_col,
        rate_rescaling = rate_rescaling,
        create_new = create_new,
        suffix = suffix,
        columns = NULL,
        skip = skip,
        id = id
      )
    )
  }

step_population_scaling_new <-
  function(role, trained, df, by, df_pop_col, rate_rescaling, terms, create_new,
           suffix, columns, skip, id) {
    recipes::step(
      subclass = "population_scaling",
      terms = terms,
      role = role,
      trained = trained,
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      rate_rescaling = rate_rescaling,
      create_new = create_new,
      suffix = suffix,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_population_scaling <- function(x, training, info = NULL, ...) {
  step_population_scaling_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    df = x$df,
    by = x$by,
    df_pop_col = x$df_pop_col,
    rate_rescaling = x$rate_rescaling,
    create_new = x$create_new,
    suffix = x$suffix,
    columns = recipes::recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_population_scaling <- function(object, new_data, ...) {
  if (is.null(object$by)) {
    rhs_potential_keys <- colnames(select(object$df, !object$df_pop_col))
    if (is_epi_df(new_data)) {
      lhs_potential_keys <- key_colnames(new_data)
      object$by <- intersect(lhs_potential_keys, rhs_potential_keys)
      suggested_min_keys <- kill_time_value(lhs_potential_keys)
      if (!all(suggested_min_keys %in% object$by)) {
        cli_warn(c(
          "Couldn't find {setdiff(suggested_min_keys, object$by)} in population `df`",
          "i" = "Defaulting to join by {object$by}",
          ">" = "Double-check whether column names on the population `df` match those for your time series",
          ">" = "Consider using population data with breakdowns by {suggested_min_keys}",
          ">" = "Manually specify `by =` to silence",
        ), class = "epipredict__step_population_scaling__default_by_missing_suggested_keys")
      }
    } else {
      object$by <- intersect(names(new_data), rhs_potential_keys)
    }
  }
  joinby <- list(x = names(object$by) %||% object$by, y = object$by)
  hardhat::validate_column_names(new_data, joinby$x)
  hardhat::validate_column_names(object$df, joinby$y)

  if (object$suffix != "_scaled" && object$create_new == FALSE) {
    cli_warn(c(
      "Custom `suffix` {.val {object$suffix}} was ignored in `step_population_scaling`.",
      i = "Perhaps `create_new` should be {.val {TRUE}}?"
    ))
  }

  object$df <- mutate(object$df, across(dplyr::where(is.character), tolower))

  pop_col <- rlang::sym(object$df_pop_col)
  suffix <- ifelse(object$create_new, object$suffix, "")
  col_to_remove <- setdiff(colnames(object$df), colnames(new_data))

  inner_join(new_data, object$df, by = object$by, suffix = c("", ".df"),
             relationship = "many-to-one", unmatched = c("error", "drop")) %>%
    mutate(
      across(
        all_of(object$columns),
        ~ .x * object$rate_rescaling / !!pop_col,
        .names = "{.col}{suffix}"
      )
    ) %>%
    # removed so the models do not use the population column
    select(!any_of(col_to_remove))
}

#' @export
print.step_population_scaling <-
  function(x, width = max(20, options()$width - 35), ...) {
    title <- "Population scaling"
    print_epi_step(x$terms, x$terms, x$trained, title, extra_text = "to rates")
    invisible(x)
  }
