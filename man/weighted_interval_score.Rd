% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_interval_score.R
\name{weighted_interval_score}
\alias{weighted_interval_score}
\title{Compute weighted interval score}
\usage{
weighted_interval_score(
  x,
  actual,
  quantile_levels = NULL,
  na_handling = c("impute", "drop", "propagate", "fail"),
  ...
)
}
\arguments{
\item{x}{A vector of class \code{quantile_pred}.}

\item{actual}{double. Actual value(s)}

\item{quantile_levels}{probabilities. If specified, the score will be
computed at this set of levels. Otherwise, those present in \code{x} will be
used.}

\item{na_handling}{character. Determines missing values are handled.
For \code{"impute"}, missing values will be
calculated if possible using the available quantiles. For \code{"drop"},
explicitly missing values are ignored in the calculation of the score, but
implicitly missing values are imputed if possible.
For \code{"propogate"}, the resulting score will be \code{NA} if any missing values
exist. Finally, if
\code{quantile_levels} is specified, \code{"fail"} will result in
the score being \code{NA} when any required quantile levels (implicit or explicit)
do not have corresponding values.}

\item{...}{not used}
}
\value{
a vector of nonnegative scores.
}
\description{
Weighted interval score (WIS), a well-known quantile-based
approximation of the commonly-used continuous ranked probability score
(CRPS). WIS is a proper score, and can be thought of as a distributional
generalization of absolute error. For example, see \href{https://arxiv.org/abs/2005.12881}{Bracher et al. (2020)} for discussion in the context
of COVID-19 forecasting.
}
\examples{
quantile_levels <- c(.2, .4, .6, .8)
predq1 <- 1:4 #
predq2 <- 8:11
dstn <- quantile_pred(rbind(predq1, predq2), quantile_levels)
actual <- c(3.3, 7.1)
weighted_interval_score(dstn, actual)
weighted_interval_score(dstn, actual, c(.25, .5, .75))

# Missing value behaviours
dstn <- quantile_pred(matrix(c(1, 2, NA, 4), nrow = 1), 1:4 / 5)
weighted_interval_score(dstn, 2.5)
weighted_interval_score(dstn, 2.5, 1:9 / 10)
weighted_interval_score(dstn, 2.5, 1:9 / 10, na_handling = "drop")
weighted_interval_score(dstn, 2.5, na_handling = "propagate")
weighted_interval_score(
  quantile_pred(matrix(1:4, nrow = 1), 1:4 / 5),
  actual = 2.5,
  quantile_levels = 1:9 / 10,
  na_handling = "fail"
)


# Using some actual forecasts --------
library(dplyr)
training <- covid_case_death_rates \%>\%
  filter(time_value >= "2021-10-01", time_value <= "2021-12-01")
preds <- flatline_forecaster(
  training, "death_rate",
  flatline_args_list(quantile_levels = c(.01, .025, 1:19 / 20, .975, .99))
)$predictions
actuals <- covid_case_death_rates \%>\%
  filter(time_value == as.Date("2021-12-01") + 7) \%>\%
  select(geo_value, time_value, actual = death_rate)
preds <- left_join(preds, actuals,
  by = c("target_date" = "time_value", "geo_value")
) \%>\%
  mutate(wis = weighted_interval_score(.pred_distn, actual))
preds
}
