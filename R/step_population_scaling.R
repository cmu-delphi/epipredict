#' Create a recipe step that scales variables using population data
#'
#' `step_population_scaling` creates a specification of a recipe step
#' that will add a population scaled column in the data. For example,
#' load a dataset that contains county population, and join to an `epi_df`
#' that currently only contains number of new cases by county. Once scaled,
#' predictions can be made on case rate. Although worth noting that there is
#' nothing special about "population". The function can be used to scale by any
#' variable. Population is simply the most natural and common use case.
#'
#' @param recipe A recipe object. The step will be added to the sequence of
#' operations for this recipe. The recipe should contain information about the
#' `epi_df` such as column names.
#' @param df a data frame that contains the population data used for scaling.
#' @param by A character vector of variables to join by.
#'
#' If `NULL`, the default, the function will perform a natural join, using all
#' variables in common across the `epi_df` and the user-provided dataset.
#' If columns in `epi_df` and `df` have the same name (and aren't
#' included in by), `.df` is added to the one from the user-provided data
#' to disambiguate.
#'
#' To join by different variables on the `epi_df` and `df`, use a named vector.
#' For example, by = c("geo_value" = "states") will match `epi_df$geo_value`
#' to `df$states`. To join by multiple variables, use a vector with length > 1.
#' For example, by = c("geo_value" = "states", "county" = "county") will match
#' `epi_df$geo_value` to `df$states` and `epi_df$county` to `df$county`.
#'
#'
#' @param df_pop_col the name of the column in the data frame `df` that
#' contains the population data and will be used for scaling.
#' This should be one column, and column names should be in lower case.
#' @param ... the corresponding column(s) in the `epi_df` that will be scaled.
#' @param inputs Quosure(s) of `...`.
#' @param create_new TRUE to create a new column and keep the original column
#' in the `epi_df`
#' @param suffix a character. The suffix added to the column name if
#' `crete_new = TRUE`. Default to "_scaled".
#' @param role For model terms created by this step, what analysis role should
#' they be assigned? By default, the new columns created by this step from the
#' original variables will be used as predictors in a model. Other options can
#' be ard are not limited to "outcome".
#' @param trained A logical to indicate if the quantities for preprocessing
#' have been estimated.
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
#' pop_data = data.frame(states = c("ca", "ny"),
#'                       value = c(20000, 30000))
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
#' wf <- epi_workflow(r,
#'                    parsnip::linear_reg()) %>%
#'   parsnip::fit(jhu) %>%
#'   add_frosting(f)
#'
#' latest <- get_test_data(recipe = r,
#'                         x = epiprocess::jhu_csse_daily_subset %>%
#'                           dplyr::filter(time_value > "2021-11-01",
#'                                         geo_value %in% c("ca", "ny")) %>%
#'                           dplyr::select(geo_value, time_value, cases))
#'
#'
#' predict(wf, latest)
step_population_scaling <-
  function(recipe,
          ...,
          df,
          by = NULL,
          df_pop_col,
          inputs = NULL,
          create_new = TRUE,
          suffix = "_scaled",
          role = "predictor",
          trained = FALSE,
          skip = FALSE,
          id = rand_id("population_scaling")){
  add_step(
    recipe,
    step_population_scaling_new(
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      terms = dplyr::enquos(...),
      inputs = inputs,
      create_new = create_new,
      suffix = suffix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  )
}

step_population_scaling_new <-
  function(df, by, df_pop_col, terms, inputs, create_new,
           suffix, role, trained, skip, id) {
    step(
      subclass = "population_scaling",
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      terms = terms,
      inputs= inputs,
      create_new = create_new,
      suffix = suffix,
      role = role,
      trained = trained,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_population_scaling <- function(x, training, info = NULL, ...) {
  step_population_scaling_new(
    df = x$df,
    by = x$by,
    df_pop_col = x$df_pop_col,
    terms = x$terms,
    inputs = recipes_eval_select(x$terms, training, info),
    create_new = x$create_new,
    suffix = x$suffix,
    role = x$role,
    trained = TRUE,
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

  if(object$suffix != "_scaled" && object$create_new == FALSE){
    message("`suffix` not used to generate new column in `step_population_scaling`")
  }

  object$df <- object$df %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        tolower))

  pop_col = rlang::sym(object$df_pop_col)
  suffix = ifelse(object$create_new, object$suffix, "")

  dplyr::left_join(new_data, object$df,
                   by= tolower(object$by),
                   suffix = c("", ".df")) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(object$inputs),
        ~.x/!!pop_col ,
        .names = "{.col}{suffix}")) %>%
    # removed so the models do not use the population column
   dplyr::select(- !!pop_col)

}


print.step_population_scaling <-
  function(x, width = max(20, options()$width - 35), ...) {
  title <- "Population scaling"
  recipes::print_step(x$inputs, x$inputs, x$trained, title, width)
  invisible(x)
}


