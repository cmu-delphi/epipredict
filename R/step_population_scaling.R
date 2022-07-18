#' Create a recipe step that scales variables using population data
#'
#' `step_population_scaling` creates a specification of a recipe step
#' that will add a population scaled column in the data. For example,
#' load a dataset that contains county population, and join to an `epi_df`
#' that currently only contains number of new cases by county. Once scaled,
#' predictions can be made on case rate.
#'
#' @param recipe A recipe object. The step will be added to the sequence of operations for this recipe. The recipe should contain information about the `epi_df` such as column names.
#' @param df a data frame that contains the population data used for scaling.
#' @param by A character vector of variables to join by. Column names should be in lower case.
#' If `NULL`, the default will perform a natural join, using all variables in common across the `epi_df` and `df`.
#' A message lists the variables so that you can check they're correct; suppress the message by supplying by explicitly.
#'
#' To join by different variables on the `epi_df` and `df`, use a named vector. For example, by = c("geo_value" = "states") will match `epi_df$geo_value` to `df$states`.
#' To join by multiple variables, use a vector with length > 1. For example, by = c("geo_value" = "states", "county" = "county") will match `epi_df$geo_value` to `df$states` and `epi_df$county` to `df$county`.
#'
#' @param df_pop_col the name of the column in the data frame `df` that contains the population data and will be used for scaling. This should be one column, and column names should be in lower case.
#' @param ... the corresponding column(s) in the `epi_df` that will be scaled.
#' @param inputs Quosure(s) of `...`.
#' @param create_new TRUE to create a new column and keep the original column in the `epi_df`
#' @param suffix a character. The suffix added to the column name if `crete_new = TRUE`. Default to "_scaled".
#' @param role For model terms created by this step, what analysis role should they be assigned? By default, the new columns created by this step from the original variables will be used as predictors in a model. Other options can be ard are not limited to "outcome".
#' @param trained A logical to indicate if the quantities for preprocessing have been estimated.
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A unique identifier for the step
#'
#' @return Creates a population scaled column in training time to fit the model.
#' @export
#' @examples
#' library(dplyr)
#' pop_data = data.frame(states = c("ak","al","ar","as","az","ca"),
#' value = c(1000, 2000, 3000, 4000, 5000, 6000))
#'
#' newdata = case_death_rate_subset %>% dplyr::filter(geo_value %in%  c("ak","al","ar","as","az","ca"))
#'
#' r <- epi_recipe(newdata) %>%
#'  step_population_scaling(df = pop_data,
#'  df_pop_col = "value",
#'  by = c("geo_value" = "states"),
#'  case_rate, death_rate)
#'
#'  prep <- prep(r, newdata)
#'
#'  bake(prep, newdata)
step_population_scaling <-
  function(recipe,
          df,
          by = NULL,
          df_pop_col,
          ...,
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
  function(df, by, df_pop_col, terms, inputs, create_new, suffix, role, trained, skip, id) {
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

  object$df <- object$df %>% dplyr::mutate(dplyr::across(where(is.character), tolower))

  pop_col = sym(object$df_pop_col)
  suffix = ifelse(object$create_new, object$suffix, "")

  dplyr::left_join(new_data, object$df, by= tolower(object$by)) %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(object$inputs), ~.x/!!pop_col , .names = "{.col}{suffix}"))

}


print.step_population_scaling <-  function(x, width = max(20, options()$width - 35), ...) {
  title <- "Population scaling"
  recipes::print_step(x$inputs, x$inputs, x$trained, title, width)
  invisible(x)
}


