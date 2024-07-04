#' @importFrom recipes recipe
#' @export
recipe.epi_df <- function(x, formula = NULL, ..., vars = NULL, roles = NULL) {
  # vars + roles must be same-length character vectors
  # formula is mutually exclusive with vars + roles
  # either determines the variables needed for modelling
  attr(x, "decay_to_tibble") <- FALSE # avoid as_tibble stripping the class
  r <- NextMethod("recipe")
  r <- add_epi_df_roles_to_recipe(r, x)

  # arrange to easy order
  r$var_info <- r$var_info %>%
    dplyr::arrange(factor(
      role,
      levels = union(
        c("predictor", "outcome", "time_value", "geo_value", "key"),
        unique(role)
      ) # anything else
    ))
  r$term_info <- r$var_info
  class(r) <- c("epi_recipe", class(r))
  r
}

#' @exportS3Method recipes::recipe
recipe.formula <- function(formula, data, ...) {
  # This method clobbers `recipes::recipe.formula`, but should have no noticible
  # effect.
  recipe(x = data, formula = formula, ...)
}

add_epi_df_roles_to_recipe <- function(r, epi_df) {
  edf_keys <- epiprocess::key_colnames(epi_df)
  edf_roles <- c("time_value", "geo_value", rep("key", length(edf_keys) - 2))
  types <- recipes:::get_types(epi_df[, edf_keys])$type
  info <- tibble(
    variable = edf_keys,
    type = types,
    role = edf_roles,
    source = "original"
  )
  # reconstruct the constituents
  r$template <- epi_df[ ,unique(c(edf_keys, r$var_info$variable))]
  r$var_info <- r$var_info %>%
    dplyr::filter(!((variable %in% edf_keys) & is.na(role))) %>%
    dplyr::bind_rows(info) %>%
    dplyr::distinct()
  r
}
