% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_quantiles.R
\name{nested_quantiles}
\alias{nested_quantiles}
\title{Turn a vector of quantile distributions into a list-col}
\usage{
nested_quantiles(x)
}
\arguments{
\item{x}{a \code{distribution} containing \code{dist_quantiles}}
}
\value{
a list-col
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}
}
\details{
This function is deprecated. The recommended alternative is
\code{\link[hardhat:quantile_pred]{hardhat::quantile_pred()}} with \code{\link[tibble:as_tibble]{tibble::as_tibble()}}
}
\examples{
pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
nested_quantiles(pred_quantile)

pred_quantile \%>\%
  as_tibble() \%>\%
  tidyr::nest(.by = .row) \%>\%
  dplyr::select(-.row)

}
