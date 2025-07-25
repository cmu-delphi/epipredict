% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pivot_quantiles.R
\name{pivot_quantiles}
\alias{pivot_quantiles}
\alias{pivot_quantiles_longer}
\alias{pivot_quantiles_wider}
\title{Pivot a column containing \code{quantile_pred} to explicit rows or columns}
\usage{
pivot_quantiles_longer(.data, ...)

pivot_quantiles_wider(.data, ...)
}
\arguments{
\item{.data}{A data frame, or a data frame extension such as a tibble or
epi_df.}

\item{...}{<\code{\link[dplyr:dplyr_tidy_select]{tidy-select}}> One unquoted
expressions separated by commas. Variable names can be used as if they
were positions in the data frame. Note that only one variable
can be selected for this operation.}
}
\value{
An object of the same class as \code{.data}.
}
\description{
Both functions expand a column of \code{quantile_pred}s into the separate
quantiles. Since each consists of a set of names (quantiles) and values,
these operate analogously with \code{pivot_wider} and \code{pivot_longer}.
}
\details{
\code{piot_quantiles_wider} creates a new column for each \code{quantile_level}, with
the values as the corresponding quantile values.  When pivoting multiple
columns, the original column name will be used as a prefix.

Similarly, \code{pivot_quantiles_longer} assigns the selected columns
\code{quantile_level}s in one column and the \code{value}s in another. If multiple
columns are selected, these will be prefixed with the column name.
}
\examples{
d1 <- quantile_pred(rbind(1:3, 2:4), 1:3 / 4)
d2 <- quantile_pred(rbind(2:4, 3:5), 2:4 / 5)
tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)

pivot_quantiles_longer(tib, "d1")
pivot_quantiles_longer(tib, dplyr::ends_with("1"))
pivot_quantiles_longer(tib, d2)

pivot_quantiles_wider(tib, "d1")
pivot_quantiles_wider(tib, dplyr::ends_with("2"))
pivot_quantiles_wider(tib, d2)
}
