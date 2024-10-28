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
    if (rlang::dots_n(...) == 0L) {
      cli_abort(c(
        "`...` must not be empty.",
        ">" = "Please provide one or more tidyselect expressions in `...`
               specifying the columns to which scaling should be applied.",
        ">" = "If you really want to list `step_population_scaling` in your
               recipe but not have it do anything, you can use a tidyselection
               that selects zero variables, such as `c()`."
      ))
    }
    arg_is_scalar(role, df_pop_col, rate_rescaling, create_new, suffix, skip, id)
    arg_is_chr(role, df_pop_col, suffix, id)
    hardhat::validate_column_names(df, df_pop_col)
    arg_is_chr(by, allow_null = TRUE)
    arg_is_numeric(rate_rescaling)
    if (rate_rescaling <= 0) {
      cli_abort("`rate_rescaling` must be a positive number.")
    }
    arg_is_lgl(create_new, skip)

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
  if (is.null(x$by)) {
    rhs_potential_keys <- setdiff(colnames(x$df), x$df_pop_col)
    lhs_potential_keys <- info %>%
      filter(role %in% c("geo_value", "key", "time_value")) %>%
      extract2("variable") %>%
      unique() # in case of weird var with multiple of above roles
    if (length(lhs_potential_keys) == 0L) {
      # We're working with a recipe and tibble, and *_role hasn't set up any of
      # the above roles. Let's say any column could actually act as a key, and
      # lean on `intersect` below to make this something reasonable.
      lhs_potential_keys <- names(training)
    }
    suggested_min_keys <- info %>%
      filter(role %in% c("geo_value", "key")) %>%
      extract2("variable") %>%
      unique()
    # (0 suggested keys if we weren't given any epikeytime var info.)
    x$by <- intersect(lhs_potential_keys, rhs_potential_keys)
    if (length(x$by) == 0L) {
      cli_stop(c(
        "Couldn't guess a default for `by`",
        ">" = "Please rename columns in your population data to match those in your training data,
               or manually specify `by =` in `step_population_scaling()`."
      ), class = "epipredict__step_population_scaling__default_by_no_intersection")
    }
    if (!all(suggested_min_keys %in% x$by)) {
      cli_warn(c(
        "Couldn't find {setdiff(suggested_min_keys, x$by)} in population `df`.",
        "i" = "Defaulting to join by {x$by}.",
        ">" = "Double-check whether column names on the population `df` match those for your time series.",
        ">" = "Consider using population data with breakdowns by {suggested_min_keys}.",
        ">" = "Manually specify `by =` to silence."
      ), class = "epipredict__step_population_scaling__default_by_missing_suggested_keys")
    }
  }
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
    cli::cli_abort(c(
      "`by` was not set and no default was filled in",
      ">" = "If this was a fit recipe generated from an older version
             of epipredict that you loaded in from a file,
             please regenerate with the current version of epipredict."
    ))
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

  inner_join(new_data, object$df,
             by = object$by, relationship = "many-to-one", unmatched = c("error", "drop"),
             suffix = c("", ".df")) %>%
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
