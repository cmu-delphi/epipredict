% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blueprint-epi_recipe-default.R
\name{new_epi_recipe_blueprint}
\alias{new_epi_recipe_blueprint}
\alias{epi_recipe_blueprint}
\alias{default_epi_recipe_blueprint}
\alias{new_default_epi_recipe_blueprint}
\title{Recipe blueprint that accounts for \code{epi_df} panel data}
\usage{
new_epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble",
  ptypes = NULL,
  recipe = NULL,
  ...,
  subclass = character()
)

epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble"
)

default_epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble"
)

new_default_epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble",
  ptypes = NULL,
  recipe = NULL,
  extra_role_ptypes = NULL,
  ...,
  subclass = character()
)
}
\arguments{
\item{intercept}{A logical. Should an intercept be included in the
processed data? This information is used by the \code{process} function
in the \code{mold} and \code{forge} function list.}

\item{allow_novel_levels}{A logical. Should novel factor levels be allowed at
prediction time? This information is used by the \code{clean} function in the
\code{forge} function list, and is passed on to \code{\link[hardhat:scream]{scream()}}.}

\item{fresh}{Should already trained operations be re-trained when \code{prep()} is
called?}

\item{composition}{Either "tibble", "matrix", or "dgCMatrix" for the format
of the processed predictors. If "matrix" or "dgCMatrix" are chosen, all of
the predictors must be numeric after the preprocessing method has been
applied; otherwise an error is thrown.}

\item{ptypes}{Either \code{NULL}, or a named list with 2 elements, \code{predictors}
and \code{outcomes}, both of which are 0-row tibbles. \code{ptypes} is generated
automatically at \code{\link[hardhat:mold]{mold()}} time and is used to validate \code{new_data} at
prediction time.}

\item{recipe}{Either \code{NULL}, or an unprepped recipe. This argument is set
automatically at \code{\link[hardhat:mold]{mold()}} time.}

\item{...}{Name-value pairs for additional elements of blueprints that
subclass this blueprint.}

\item{subclass}{A character vector. The subclasses of this blueprint.}

\item{extra_role_ptypes}{A named list. The names are the unique non-standard
recipe roles (i.e. everything except \code{"predictors"} and \code{"outcomes"}). The
values are prototypes of the original columns with that role. These are
used for validation in \code{forge()}.}
}
\value{
A recipe blueprint.
}
\description{
Used for simplicity. See \code{\link[hardhat:new-blueprint]{hardhat::new_recipe_blueprint()}} or
\code{\link[hardhat:default_recipe_blueprint]{hardhat::default_recipe_blueprint()}} for more details.
}
\details{
The \code{bake_dependent_roles} are automatically set to \code{epi_df} defaults.
}
\keyword{internal}
