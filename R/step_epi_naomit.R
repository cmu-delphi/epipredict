#' Unified NA omission wrapper function for recipes
#'
#' @param recipe Recipe to be used for omission steps
#'
#' @return Omits NA's from both predictors and outcomes at training time;
#'   however, only omits associated predictors at prediction time to avoid
#'   losing data.
#' @export
#' @examples
#' tibble(geo_value = rep("place",200),
#'            time_value = as.Date("2021-01-01") + 0:199,
#'            case_rate = 1:200,
#'            death_rate = 1:200) %>%
#'  as_epi_df() %>%
#'  recipe() %>%
#'  step_epi_naomit()


step_epi_naomit <- function(recipe) {
  stopifnot("recipe" %in% class(recipe))
  recipe %>%
    recipes::step_naomit(all_predictors()) %>%
    recipes::step_naomit(all_outcomes(), skip = TRUE)
}
