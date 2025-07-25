% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/layer_add_forecast_date.R
\name{layer_add_forecast_date}
\alias{layer_add_forecast_date}
\title{Post-processing step to add the forecast date}
\usage{
layer_add_forecast_date(
  frosting,
  forecast_date = NULL,
  id = rand_id("add_forecast_date")
)
}
\arguments{
\item{frosting}{a \code{frosting} postprocessor}

\item{forecast_date}{The forecast date to add as a column to the \code{epi_df}.
For most cases, this should be specified in the form "yyyy-mm-dd". Note
that when the forecast date is left unspecified, it is set to one of two
values.  If there is a \code{step_adjust_latency} step present, it uses the
\code{forecast_date} as set in that function. Otherwise, it uses the maximum
\code{time_value} across the data used for pre-processing, fitting the model,
and post-processing.}

\item{id}{a random id string}
}
\value{
an updated \code{frosting} postprocessor
}
\description{
Post-processing step to add the forecast date
}
\details{
To use this function, either specify a forecast date or leave the
forecast date unspecifed here. In the latter case, the forecast date will
be set as the maximum time value from the data used in pre-processing,
fitting the model, and post-processing. In any case, when the forecast date is
less than the maximum \code{as_of} value (from the data used pre-processing,
model fitting, and post-processing), an appropriate warning will be thrown.
}
\examples{
jhu <- covid_case_death_rates \%>\%
  filter(time_value > "2021-11-01", geo_value \%in\% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) \%>\%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) \%>\%
  step_epi_ahead(death_rate, ahead = 7) \%>\%
  step_epi_naomit()
wf <- epi_workflow(r, linear_reg()) \%>\% fit(jhu)
latest <- jhu \%>\%
  filter(time_value >= max(time_value) - 14)

# Don't specify `forecast_date` (by default, this should be last date in latest)
f <- frosting() \%>\%
  layer_predict() \%>\%
  layer_naomit(.pred)
wf0 <- wf \%>\% add_frosting(f)
p0 <- predict(wf0, latest)
p0

# Specify a `forecast_date` that is greater than or equal to `as_of` date
f <- frosting() \%>\%
  layer_predict() \%>\%
  layer_add_forecast_date(forecast_date = "2022-05-31") \%>\%
  layer_naomit(.pred)
wf1 <- wf \%>\% add_frosting(f)

p1 <- predict(wf1, latest)
p1

# Specify a `forecast_date` that is less than `as_of` date
f2 <- frosting() \%>\%
  layer_predict() \%>\%
  layer_add_forecast_date(forecast_date = "2021-12-31") \%>\%
  layer_naomit(.pred)
wf2 <- wf \%>\% add_frosting(f2)

p2 <- predict(wf2, latest)
p2

# Do not specify a forecast_date
f3 <- frosting() \%>\%
  layer_predict() \%>\%
  layer_add_forecast_date() \%>\%
  layer_naomit(.pred)
wf3 <- wf \%>\% add_frosting(f3)

p3 <- predict(wf3, latest)
p3
}
