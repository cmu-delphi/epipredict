#' Create a recipe for preprocessing panel data
#'
#' A recipe is a description of the steps to be applied to a data set in
#'   order to prepare it for data analysis. This is an S3 method for
#'  [recipes::recipe()] to properly handle the additional (panel data)
#'   columns present in an [`epiprocess::epi_df`]: `time_value`, `geo_value`, and any
#'   additional keys.
#'
#' @aliases epi_recipe epi_recipe.default epi_recipe.formula
#' @inheritParams recipes::recipe
#' @param roles A character string (the same length of `vars`) that
#'   describes a single role that the variable will take. This value could be
#'   anything but common roles are `"outcome"`, `"predictor"`,
#'   `"time_value"`, and `"geo_value"`
#' @param ... Further arguments passed to or from other methods (not currently
#'   used).
#' @param formula A model formula. No in-line functions should be used here
#'  (e.g. `log(x)`, `x:y`, etc.) and minus signs are not allowed. These types of
#'  transformations should be enacted using `step` functions in this package.
#'  Dots are allowed as are simple multivariate outcome terms (i.e. no need for
#'  `cbind`; see Examples).
#' @param x,data A data frame, tibble, or epi_df of the *template* data set
#'   (see below). This is always coerced to the first row to avoid memory issues
#' @inherit recipes::recipe return
#'
#' @export
#' @examples
#' library(dplyr)
#' library(recipes)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-08-01") %>%
#'   arrange(geo_value, time_value)
#'
#' r <- recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_naomit(recipes::all_predictors()) %>%
#'   # below, `skip` means we don't do this at predict time
#'   step_naomit(recipes::all_outcomes(), skip = TRUE)
#'
#' r
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
#' @rdname recipe.epi_df
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
  r$template <- epi_df[, unique(c(edf_keys, r$var_info$variable))]
  r$var_info <- r$var_info %>%
    dplyr::filter(!((variable %in% edf_keys) & is.na(role))) %>%
    dplyr::bind_rows(info) %>%
    dplyr::distinct()
  r
}
