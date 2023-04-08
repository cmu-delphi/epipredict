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
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe. The recipe should contain information about the
#' `epi_df` such as column names.
#' @param ... One or more selector functions to scale variables
#'  for this step. See [recipes::selections()] for more details.
#' @param role For model terms created by this step, what analysis role should
#' they be assigned? By default, the new columns created by this step from the
#' original variables will be used as predictors in a model. Other options can
#' be ard are not limited to "outcome".
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
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
#' `crete_new = TRUE`. Default to "_scaled".
#' @param columns A character string of variable names that will
#'  be populated (eventually) by the `terms` argument.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A unique identifier for the step
#'
#' @return Scales raw data by the population
#' @export
#' @examples
#' library(epiprocess)
#' library(epipredict)
#' jhu <- epiprocess::jhu_csse_daily_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
#'   dplyr::select(geo_value, time_value, cases)
#'
#' pop_data = data.frame(states = c("ca", "ny"), value = c(20000, 30000))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_population_scaling(df = pop_data,
#'                           df_pop_col = "value",
#'                           by = c("geo_value" = "states"),
#'                           cases, suffix = "_scaled") %>%
#'   step_epi_lag(cases_scaled, lag = c(7, 14)) %>%
#'   step_epi_ahead(cases_scaled, ahead = 7, role = "outcome") %>%
#'   step_epi_naomit()
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_threshold(.pred) %>%
#'   layer_naomit(.pred) %>%
#'   layer_population_scaling(.pred, df = pop_data,
#'                            by =  c("geo_value" = "states"),
#'                            df_pop_col = "value")
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>%
#'   fit(jhu) %>%
#'   add_frosting(f)
#'
#' latest <- get_test_data(
#'   recipe = r,
#'   epiprocess::jhu_csse_daily_subset %>%
#'     dplyr::filter(time_value > "2021-11-01",
#'       geo_value %in% c("ca", "ny")) %>%
#'     dplyr::select(geo_value, time_value, cases)
#' )
#'
#'
#' predict(wf, latest)
step_population_scaling <-
  function(recipe,
          ...,
          role = "predictor",
          trained = FALSE,
          df,
          by = NULL,
          df_pop_col,
          rate_rescaling = 1,
          create_new = TRUE,
          suffix = "_scaled",
          columns = NULL,
          skip = FALSE,
          id = rand_id("population_scaling")){
  arg_is_scalar(role, trained, df_pop_col, rate_rescaling, create_new, suffix, id)
  arg_is_lgl(create_new, skip)
  arg_is_chr(df_pop_col, suffix, id)
  arg_is_chr(by, columns, allow_null = TRUE)
  if (rate_rescaling <= 0)
    cli_stop("`rate_rescaling` should be a positive number")

  add_step(
    recipe,
    step_population_scaling_new(
      terms = dplyr::enquos(...),
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
  )
}

step_population_scaling_new <-
  function(role, trained, df, by, df_pop_col, rate_rescaling, terms, create_new,
           suffix, columns, skip, id) {
    step(
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
    columns = recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' @export
bake.step_population_scaling <- function(object,
                                 new_data,
                                 ...) {

  stopifnot("Only one population column allowed for scaling" =
              length(object$df_pop_col) == 1)

  try_join <- try(dplyr::left_join(new_data, object$df, by = object$by),
                silent = TRUE)
  if (any(grepl("Join columns must be present in data", unlist(try_join)))) {
    cli_stop(c("columns in `by` selectors of `step_population_scaling` ",
               "must be present in data and match"))}

  if (object$suffix != "_scaled" && object$create_new == FALSE) {
    message("`suffix` not used to generate new column in `step_population_scaling`")
  }

  object$df <- object$df %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), tolower))

  pop_col = rlang::sym(object$df_pop_col)
  suffix = ifelse(object$create_new, object$suffix, "")

  dplyr::left_join(new_data,
                   object$df,
                   by = object$by, suffix = c("", ".df")) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(object$columns),
        ~.x * object$rate_rescaling /!!pop_col ,
        .names = "{.col}{suffix}")) %>%
    # removed so the models do not use the population column
   dplyr::select(-!!pop_col)

}

#' @export
print.step_population_scaling <-
  function(x, width = max(20, options()$width - 35), ...) {
  title <- "Population scaling"
  print_epi_step(x$terms, x$terms, x$trained, title, extra_text = "to rates")
  invisible(x)
}


