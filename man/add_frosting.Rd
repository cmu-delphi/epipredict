% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/frosting.R
\name{add_frosting}
\alias{add_frosting}
\alias{remove_frosting}
\alias{update_frosting}
\title{Add/remove/update the \code{frosting} of an \code{epi_workflow}}
\usage{
add_frosting(x, frosting, ...)

remove_frosting(x)

update_frosting(x, frosting, ...)
}
\arguments{
\item{x}{A workflow}

\item{frosting}{A frosting object created using \code{frosting()}.}

\item{...}{Not used.}
}
\value{
\code{x}, updated with a new frosting postprocessor
}
\description{
Add/remove/update the \code{frosting} of an \code{epi_workflow}
}
\examples{
jhu <- covid_case_death_rates \%>\%
  filter(time_value > "2021-11-01", geo_value \%in\% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) \%>\%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) \%>\%
  step_epi_ahead(death_rate, ahead = 7)

wf <- epi_workflow(r, linear_reg()) \%>\% fit(jhu)
latest <- jhu \%>\%
  filter(time_value >= max(time_value) - 14)

# Add frosting to a workflow and predict
f <- frosting() \%>\%
  layer_predict() \%>\%
  layer_naomit(.pred)
wf1 <- wf \%>\% add_frosting(f)
p1 <- predict(wf1, latest)
p1

# Update frosting in a workflow and predict
f2 <- frosting() \%>\% layer_predict()
wf2 <- wf1 \%>\% update_frosting(f2)
p2 <- predict(wf2, latest)
p2

# Remove frosting from the workflow and predict
wf3 <- wf2 \%>\% remove_frosting()
p3 <- predict(wf3, latest)
p3

}
