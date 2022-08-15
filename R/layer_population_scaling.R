#' Revert population scaled prediction
#'
#' `layer_population_scaling` creates a specification of a frosting layer
#' that will add a population scaled column in the data. For example,
#' load a dataset that contains county population, and join to an `epi_df`
#' that currently predicts number of new cases by county to obtain case rates.
#' Although worth noting that there is nothing special about "population".
#' The function can be used to scale by any variable. Population is simply the
#' most natural and common use case.
#'
#' @param frosting a `frosting` postprocessor. The layer will be added to the
#'  sequence of operations  for this frosting.
#' @param ... One or more selector functions to scale variables
#'  for this step. See [selections()] for more details.
#' @param df a data frame that contains the population data used for scaling.
#' @param by A character vector of variables to left join by.
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
#' See [dplyr::left_join()] for more details.
#' @param df_pop_col the name of the column in the data frame `df` that
#' contains the population data and used for scaling.
#' @param create_new TRUE to create a new column and keep the original column
#' in the `epi_df`.
#' @param suffix a character. The suffix added to the column name if
#' `create_new = TRUE`. Default to "_original".
#' @param .flag a logical to determine if the layer is added. Passed on to
#'   `add_layer()`. Default `TRUE`.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' library(epiprocess)
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
layer_population_scaling <- function(frosting,
                           ...,
                           df,
                           by = NULL,
                           df_pop_col,
                           create_new = TRUE,
                           suffix = "_original",
                           .flag = TRUE,
                           id = rand_id("population_scaling")) {

  add_layer(
    frosting,
    layer_population_scaling_new(
      df = df,
      by = by,
      df_pop_col = df_pop_col,
      terms = dplyr::enquos(...),
      create_new = create_new,
      suffix = suffix,
      id = id
    ),
    flag = .flag
  )
}

layer_population_scaling_new <-
  function(df, by, df_pop_col, terms, create_new, suffix, id) {
  layer("population_scaling",
        df = df,
        by = by,
        df_pop_col = df_pop_col,
        terms = terms,
        create_new = create_new,
        suffix = suffix,
        id = id)
}

#' @export
slather.layer_population_scaling <-
  function(object, components, the_fit, the_recipe, ...) {
    stopifnot("Only one population column allowed for scaling" =
                length(object$df_pop_col) == 1)

    try_join <- try(dplyr::left_join(components$predictions, object$df,
                              by= object$by),
             silent = TRUE)
    if (any(grepl("Join columns must be present in data", unlist(try_join)))){
      stop("columns in `by` selectors of `layer_population_scaling` must be present in data and match")}

    object$df <- object$df %>%
      dplyr::mutate(dplyr::across(where(is.character), tolower))
    pop_col = rlang::sym(object$df_pop_col)
    exprs <- rlang::expr(c(!!!object$terms))
    pos <- tidyselect::eval_select(exprs, components$predictions)
    col_names <- names(pos)
    suffix = ifelse(object$create_new, object$suffix, "")


    components$predictions <- dplyr::left_join(components$predictions,
                                               object$df,
                                               by= object$by,
                                               suffix = c("", ".df")) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(col_names),
                                  ~.x * !!pop_col ,
                                  .names = "{.col}{suffix}")) %>%
     dplyr::select(- !!pop_col)
    components
}
