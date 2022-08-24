
#' Recipe blueprint that accounts for `epi_df` panel data
#'
#' Used for simplicity. See [hardhat::new_recipe_blueprint()] or
#' [hardhat::default_recipe_blueprint()] for more details.
#'
#' @inheritParams hardhat::new_recipe_blueprint
#'
#' @details The `bake_dependent_roles` are automatically set to `epi_df` defaults.
#' @return A recipe blueprint.
#'
#' @keywords internal
#' @export
new_epi_recipe_blueprint <-
  function(intercept = FALSE, allow_novel_levels = FALSE, fresh = TRUE,
           composition = "tibble",
           ptypes = NULL, recipe = NULL, ..., subclass = character()) {
  hardhat::new_recipe_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    composition = composition,
    ptypes = ptypes,
    recipe = recipe,
    ...,
    subclass = c(subclass, "epi_recipe_blueprint")
  )
}


#' @rdname new_epi_recipe_blueprint
#' @export
epi_recipe_blueprint <-
  function(intercept = FALSE, allow_novel_levels = FALSE,
           fresh = TRUE,
           composition = "tibble") {
    new_epi_recipe_blueprint(intercept = intercept,
                             allow_novel_levels = allow_novel_levels,
                             fresh = fresh,
                             composition = composition)
  }

#' @rdname new_epi_recipe_blueprint
#' @export
default_epi_recipe_blueprint <-
  function(intercept = FALSE, allow_novel_levels = FALSE, fresh = TRUE,
           composition = "tibble") {
    new_default_epi_recipe_blueprint(
      intercept = intercept,
      allow_novel_levels = allow_novel_levels,
      fresh = fresh,
      composition = composition
    )
  }

#' @rdname new_epi_recipe_blueprint
#' @inheritParams hardhat::new_default_recipe_blueprint
#' @export
new_default_epi_recipe_blueprint <-
  function(intercept = FALSE, allow_novel_levels = FALSE,
           fresh = TRUE,
           composition = "tibble", ptypes = NULL, recipe = NULL,
           extra_role_ptypes = NULL, ..., subclass = character()) {
  new_epi_recipe_blueprint(
    intercept = intercept,
    allow_novel_levels = allow_novel_levels,
    fresh = fresh,
    composition = composition,
    ptypes = ptypes,
    recipe = recipe,
    extra_role_ptypes = extra_role_ptypes,
    ...,
    subclass = c(subclass, "default_epi_recipe_blueprint", "default_recipe_blueprint")
  )
}

#' @importFrom hardhat run_mold
#' @export
run_mold.default_epi_recipe_blueprint <- function(blueprint, ..., data) {
  rlang::check_dots_empty0(...)
  # blueprint <- hardhat:::patch_recipe_default_blueprint(blueprint)
  cleaned <- mold_epi_recipe_default_clean(blueprint = blueprint, data = data)
  blueprint <- cleaned$blueprint
  data <- cleaned$data
  hardhat:::mold_recipe_default_process(blueprint = blueprint, data = data)
}

mold_epi_recipe_default_clean <- function(blueprint, data) {
  data <- er_check_is_data_like(data)
  hardhat:::new_mold_clean(blueprint, data)
}

#' @importFrom hardhat refresh_blueprint
#' @export
refresh_blueprint.default_epi_recipe_blueprint <- function(blueprint) {
  do.call(new_default_epi_recipe_blueprint, as.list(blueprint))
}

er_check_is_data_like <- function(.x, .x_nm) {
  if (rlang::is_missing(.x_nm)) {
    .x_nm <- rlang::as_label(rlang::enexpr(.x))
  }
  if (!hardhat:::is_new_data_like(.x)) {
    hardhat:::glubort("`{.x_nm}` must be a data.frame or a matrix, not a {class1(.x)}.")
  }
  .x
}
