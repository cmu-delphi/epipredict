#' Default epi_recipe blueprint
#'
#' Recipe blueprint that accounts for `epi_df` panel data
#' Used for simplicity. See [hardhat::default_recipe_blueprint()] for more
#' details. This subclass is nearly the same, except it ensures that
#' downstream processing doesn't drop the epi_df class from the data.
#'
#' @inheritParams hardhat::default_recipe_blueprint
#' @return A `epi_recipe` blueprint.
#' @export
#' @keywords internal
default_epi_recipe_blueprint <- function(intercept = FALSE,
                                         allow_novel_levels = FALSE,
                                         fresh = TRUE,
                                         strings_as_factors = FALSE,
                                         composition = "tibble") {
  new_default_epi_recipe_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    strings_as_factors = strings_as_factors,
    composition = composition
  )
}

new_default_epi_recipe_blueprint <- function(intercept = FALSE,
                                             allow_novel_levels = TRUE,
                                             fresh = TRUE,
                                             strings_as_factors = FALSE,
                                             composition = "tibble",
                                             ptypes = NULL,
                                             recipe = NULL,
                                             extra_role_ptypes = NULL,
                                             ...,
                                             subclass = character()) {
  hardhat::new_recipe_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    strings_as_factors = strings_as_factors,
    composition = composition,
    ptypes = ptypes,
    recipe = recipe,
    extra_role_ptypes = extra_role_ptypes,
    ...,
    subclass = c(subclass, "default_epi_recipe_blueprint", "default_recipe_blueprint"))
}


#' @importFrom hardhat run_mold
#' @export
run_mold.default_epi_recipe_blueprint <- function(blueprint, ..., data) {
  rlang::check_dots_empty0(...)
  # we don't do the "cleaning" in `hardhat:::run_mold.default_recipe_blueprint`
  # That function drops the epi_df class without any recourse.
  # The only way we should be here at all is if `data` is an epi_df, but just
  # in case...
  if (!is_epi_df(data)) {
    cli_warn("`data` is not an {.cls epi_df}. It has class {.cls {class(data)}}.")
  }
  hardhat:::mold_recipe_default_process(blueprint = blueprint, data = data)
}

#' @importFrom hardhat refresh_blueprint
#' @export
refresh_blueprint.default_epi_recipe_blueprint <- function(blueprint) {
  do.call(new_default_epi_recipe_blueprint, as.list(blueprint))
}

