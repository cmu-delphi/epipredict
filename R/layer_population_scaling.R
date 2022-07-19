#' Revert population scaled prediction
#'
#' @param frosting a `frosting` postprocessor
#' @param ... the column(s) in the `epi_df` to scale back.
#' @param df a data frame that contains the population data used for scaling.
#' @param by A character vector of variables to join by. Column names should be
#' in lower case.
#' To join by different variables on the `epi_df` and `df`, use a named vector.
#' For example, by = c("geo_value" = "states") will match `epi_df$geo_value`
#' to `df$states`. To join by multiple variables, use a vector with length > 1.
#' For example, by = c("geo_value" = "states", "county" = "county") will match
#' `epi_df$geo_value` to `df$states` and `epi_df$county` to `df$county`.
#'
#' @param df_pop_col the name of the column in the data frame `df` that
#' contains the population data and will be used for scaling.
#' @param create_new TRUE to create a new column and keep the original column
#' in the `epi_df`.
#' @param suffix a character. The suffix added to the column name if
#' `crete_new = TRUE`. Default to "_original".
#' @param .flag a logical to determine if the layer is added. Passed on to
#'   `add_layer()`. Default `TRUE`.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with population scaled variables
#' reverted back to the original variable.
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

    object$df <- object$df %>%
      dplyr::mutate(dplyr::across(where(is.character), tolower))

    pop_col = rlang::sym(object$df_pop_col)

    exprs <- rlang::expr(c(!!!object$terms))
    pos <- tidyselect::eval_select(exprs, components$predictions)
    col_names <- names(pos)

    suffix = ifelse(object$create_new, object$suffix, "")

    components$predictions <- dplyr::left_join(components$predictions,
                                               object$df,
                                               by= tolower(object$by)) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(col_names),
                                  ~.x * !!pop_col ,
                                  .names = "{.col}{suffix}")) %>%
      dplyr::select(- !!pop_col)

    components


}
