#' Special NA omit step that does two steps in one
#'
#' @param x Recipe to be used.
#'
#' @return A recipe with NA's omitted ....
#' @export recipes

step_naomit2 <- function(x) {
  x %>%
    recipes::step_naomit(all_predictors()) %>%
    recipes::step_naomit(all_outcomes(), skip = TRUE)
}
