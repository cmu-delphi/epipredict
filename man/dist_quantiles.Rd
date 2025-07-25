% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quantile_pred-methods.R
\name{dist_quantiles}
\alias{dist_quantiles}
\title{A distribution parameterized by a set of quantiles}
\usage{
dist_quantiles(values, quantile_levels)
}
\arguments{
\item{values}{A vector (or list of vectors) of values.}

\item{quantile_levels}{A vector (or list of vectors) of probabilities
corresponding to \code{values}.

When creating multiple sets of \code{values}/\code{quantile_levels} resulting in
different distributions, the sizes must match. See the examples below.}
}
\value{
A vector of class \code{"distribution"}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\details{
This function is deprecated. The recommended alternative is
\code{\link[hardhat:quantile_pred]{hardhat::quantile_pred()}}.
}
\keyword{internal}
