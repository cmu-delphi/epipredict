% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/step_growth_rate.R
\name{step_growth_rate}
\alias{step_growth_rate}
\title{Calculate a growth rate}
\usage{
step_growth_rate(
  recipe,
  ...,
  role = "predictor",
  horizon = 7,
  method = c("rel_change", "linear_reg"),
  log_scale = FALSE,
  na_rm = TRUE,
  replace_Inf = NA,
  prefix = "gr_",
  skip = FALSE,
  id = rand_id("growth_rate")
)
}
\arguments{
\item{recipe}{A recipe object. The step will be added to the
sequence of operations for this recipe.}

\item{...}{One or more selector functions to choose variables
for this step. See \code{\link[recipes:selections]{recipes::selections()}} for more details.}

\item{role}{For model terms created by this step, what analysis role should
they be assigned? \code{lag} is default a predictor while \code{ahead} is an outcome.}

\item{horizon}{Bandwidth for the sliding window, when \code{method} is
"rel_change" or "linear_reg". See \code{\link[epiprocess:growth_rate]{epiprocess::growth_rate()}} for more
details.}

\item{method}{Either "rel_change" or "linear_reg",
indicating the method to use for the growth rate
calculation. These are local methods: they are run in a sliding
fashion over the sequence (in order to estimate derivatives and hence
growth rates). See \code{\link[epiprocess:growth_rate]{epiprocess::growth_rate()}} for more details.}

\item{log_scale}{Should growth rates be estimated using the parameterization
on the log scale? See details for an explanation. Default is \code{FALSE}.}

\item{na_rm}{Should missing values be removed before the computation? Default
is \code{FALSE}.}

\item{replace_Inf}{Sometimes, the growth rate calculation can result in
infinite values (if the denominator is zero, for example). In this case,
most prediction methods will fail. This argument specifies potential
replacement values. The default (\code{NA}) will likely result in these rows
being removed from the data. Alternatively, you could specify arbitrary
large values, or perhaps zero. Setting this argument to \code{NULL} will result
in no replacement.}

\item{prefix}{A character string that will be prefixed to the new column.}

\item{skip}{A logical. Should the step be skipped when the
recipe is baked by \code{\link[=bake]{bake()}}? While all operations are baked
when \code{\link[=prep]{prep()}} is run, some operations may not be able to be
conducted on new data (e.g. processing the outcome variable(s)).
Care should be taken when using \code{skip = TRUE} as it may affect
the computations for subsequent operations.}

\item{id}{A unique identifier for the step}
}
\value{
An updated version of \code{recipe} with the new step added to the
sequence of any existing operations.
}
\description{
\code{step_growth_rate()} creates a \emph{specification} of a recipe step that will
generate one or more new columns of derived data. This is a wrapper around
\code{epiprocess::growth_rate()} to allow its use within an \code{epi_recipe()}.
}
\examples{
tiny_geos <- c("as", "mp", "vi", "gu", "pr")
rates <- covid_case_death_rates \%>\%
  filter(time_value >= as.Date("2021-11-01"), !(geo_value \%in\% tiny_geos))

r <- epi_recipe(rates) \%>\%
  step_growth_rate(case_rate, death_rate)
r

r \%>\%
  prep(rates) \%>\%
  bake(new_data = NULL)
}
\seealso{
Other row operation steps: 
\code{\link{step_adjust_latency}()},
\code{\link{step_epi_lag}()},
\code{\link{step_lag_difference}()}
}
\concept{row operation steps}
