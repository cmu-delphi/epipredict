% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer_threshold_preds.R
\name{layer_threshold}
\alias{layer_threshold}
\title{Lower and upper thresholds for predicted values}
\usage{
layer_threshold(
  frosting,
  ...,
  lower = 0,
  upper = Inf,
  id = rand_id("threshold")
)
}
\arguments{
\item{frosting}{a \code{frosting} postprocessor}

\item{...}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> One or more unquoted
expressions separated by commas. Variable names can be used as if they
were positions in the data frame, so expressions like \code{x:y} can
be used to select a range of variables. Typical usage is \code{.pred} to
threshold predictions to a range (say, nonnegative).}

\item{lower}{Lower threshold for the prediction values. That is, any
predictions that are less than this lower bound are set to it.
Default value is \code{0}.}

\item{upper}{Upper threshold for the prediction values. That is, any
predictions that are greater than this upper bound are set to it.
Default value is \code{Inf}.}

\item{id}{a random id string}
}
\value{
an updated \code{frosting} postprocessor
}
\description{
This post-processing step is used to set prediction values that are smaller
than the lower threshold or higher than the upper threshold equal to the
threshold values.
}
\details{
Making case count predictions strictly positive is a typical example usage.
It must be called after there is a column containing quantiles. This means at earliest it can be called after \code{layer_predict()} for distributional models, or after \code{layer_residual_quantiles()} for point prediction models. Typical best practice will use \code{starts_with(".pred")} as the variables to threshold.
}
\examples{
jhu <- covid_case_death_rates \%>\%
  filter(time_value < "2021-03-08", geo_value \%in\% c("ak", "ca", "ar"))

r <- epi_recipe(jhu) \%>\%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) \%>\%
  step_epi_ahead(death_rate, ahead = 7) \%>\%
  step_epi_naomit()
wf <- epi_workflow(r, linear_reg()) \%>\% fit(jhu)

f <- frosting() \%>\%
  layer_predict() \%>\%
  layer_threshold(starts_with(".pred"), lower = 0.180, upper = 0.310)
wf <- wf \%>\% add_frosting(f)
p <- forecast(wf)
p
}
