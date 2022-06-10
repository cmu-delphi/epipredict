#' Unified NA omission wrapper function for recipes
#'
#' @param recipe Recipe to be used for omission steps
#'
#' @return Omits NA's from both predictors and outcomes at training time
#'   to fit the model. Also only omits associated predictors and not
#'   outcomes at prediction time due to lack of response and avoidance
#'   of data loss.
#' @export
#' @examples
#' case_death_rate_subset %>%
#'  recipe() %>%
#'  step_epi_naomit()

step_epi_naomit <- function(recipe) {
  stopifnot("recipe" %in% class(recipe))
  recipe %>%
    recipes::step_naomit(all_predictors()) %>%
    recipes::step_naomit(all_outcomes(), skip = TRUE)
}
