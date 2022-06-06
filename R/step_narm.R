#' Special NA omit step that does two steps in one
#'
#' @param x Recipe to be used for omission steps
#'
#' @return Omits NA's from both predictors and outcomes and training time;
#' however, only omits predictors at prediction time
#' @export

step_narm <- function(x) {
  x %>%
    recipes::step_naomit(all_predictors()) %>%
    recipes::step_naomit(all_outcomes(), skip = TRUE)
}
