% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/weighted_interval_score.R
\name{weighted_interval_score}
\alias{weighted_interval_score}
\alias{weighted_interval_score.dist_quantiles}
\title{Compute weighted interval score}
\usage{
weighted_interval_score(x, actual, quantile_levels = NULL, ...)

\method{weighted_interval_score}{dist_quantiles}(
  x,
  actual,
  quantile_levels = NULL,
  na_handling = c("impute", "drop", "propagate", "fail"),
  ...
)
}
\arguments{
\item{x}{distribution. A vector of class distribution. Ideally, this vector
contains \code{dist_quantiles()}, though other distributions are supported when
\code{quantile_levels} is specified. See below.}

\item{actual}{double. Actual value(s)}

\item{quantile_levels}{probabilities. If specified, the score will be
computed at this set of levels.}

\item{...}{not used}

\item{na_handling}{character. Determines how \code{quantile_levels} without a
corresponding \code{value} are handled. For \code{"impute"}, missing values will be
calculated if possible using the available quantiles. For \code{"drop"},
explicitly missing values are ignored in the calculation of the score, but
implicitly missing values are imputed if possible.
For \code{"propogate"}, the resulting score will be \code{NA} if any missing values
exist in the original \code{quantile_levels}. Finally, if
\code{quantile_levels} is specified, \code{"fail"} will result in
the score being \code{NA} when any required quantile levels (implicit or explicit)
are do not have corresponding values.}
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
\section{Methods (by class)}{
\itemize{
\item \code{weighted_interval_score(dist_quantiles)}: Weighted interval score with
\code{dist_quantiles} allows for different \code{NA} behaviours.

}}
\examples{
quantile_levels <- c(.2, .4, .6, .8)
predq_1 <- 1:4 #
predq_2 <- 8:11
dstn <- dist_quantiles(list(predq_1, predq_2), quantile_levels)
actual <- c(3.3, 7.1)
weighted_interval_score(dstn, actual)
weighted_interval_score(dstn, actual, c(.25, .5, .75))

library(distributional)
dstn <- dist_normal(c(.75, 2))
weighted_interval_score(dstn, 1, c(.25, .5, .75))

# Missing value behaviours
dstn <- dist_quantiles(c(1, 2, NA, 4), 1:4 / 5)
weighted_interval_score(dstn, 2.5)
weighted_interval_score(dstn, 2.5, 1:9 / 10)
weighted_interval_score(dstn, 2.5, 1:9 / 10, na_handling = "drop")
weighted_interval_score(dstn, 2.5, na_handling = "propagate")
weighted_interval_score(dist_quantiles(1:4, 1:4 / 5), 2.5, 1:9 / 10,
  na_handling = "fail"
)


# Using some actual forecasts --------
library(dplyr)
jhu <- case_death_rate_subset \%>\%
  filter(time_value >= "2021-10-01", time_value <= "2021-12-01")
preds <- flatline_forecaster(
  jhu, "death_rate",
  flatline_args_list(quantile_levels = c(.01, .025, 1:19 / 20, .975, .99))
)$predictions
actuals <- case_death_rate_subset \%>\%
  filter(time_value == as.Date("2021-12-01") + 7) \%>\%
  select(geo_value, time_value, actual = death_rate)
preds <- left_join(preds, actuals,
  by = c("target_date" = "time_value", "geo_value")
) \%>\%
  mutate(wis = weighted_interval_score(.pred_distn, actual))
preds
}
