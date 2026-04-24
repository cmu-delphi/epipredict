# Quantiles from a distribution

Given a
[hardhat::quantile_pred](https://hardhat.tidymodels.org/reference/quantile_pred.html)
object, users may wish to compute additional `quantile_levels` that are
not part of the object. This function attempts to estimate these
quantities under some assumptions. Interior probabilities, those
contained within existing probabilities are interpolated in a manner
controled by the `middle` argument. Those outside existing probabilities
are extrapolated under the assumption that the tails of the distribution
decays exponentially. Optionally, one may constrain *all* quantiles to
be within some support (say, `[0, Inf)`).

## Usage

``` r
# S3 method for class 'quantile_pred'
quantile(
  x,
  probs = seq(0, 1, 0.25),
  na.rm = FALSE,
  lower = -Inf,
  upper = Inf,
  middle = c("cubic", "linear"),
  ...
)
```

## Arguments

- x:

  numeric vector whose sample quantiles are wanted, or an object of a
  class for which a method has been defined (see also ‘details’).
  [`NA`](https://rdrr.io/r/base/NA.html) and `NaN` values are not
  allowed in numeric vectors unless `na.rm` is `TRUE`.

- probs:

  numeric vector of probabilities with values in \\\[0,1\]\\. (Values up
  to `2e-14` outside that range are accepted and moved to the nearby
  endpoint.)

- na.rm:

  logical; if true, any [`NA`](https://rdrr.io/r/base/NA.html) and
  `NaN`'s are removed from `x` before the quantiles are computed.

- lower:

  Scalar. Optional lower bound.

- upper:

  Scalar. Optional upper bound.

- middle:

  Controls how extrapolation to "interior" probabilities is performed.
  "cubic" attempts to use
  [`stats::splinefun()`](https://rdrr.io/r/stats/splinefun.html) while
  "linear" uses
  [`stats::approx()`](https://rdrr.io/r/stats/approxfun.html). The
  "linear" method is used as a fallback if "cubic" should fail for some
  reason.

- ...:

  unused

## Value

a matrix with one row for each entry in `x` and one column for each
value in `probs`

## See also

[`extrapolate_quantiles()`](https://cmu-delphi.github.io/epipredict/dev/reference/extrapolate_quantiles.md)

## Examples

``` r
qp <- quantile_pred(matrix(1:8, nrow = 2, byrow = TRUE), 1:4 / 5)
quantile(qp)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] -Inf 1.25  2.5 3.75  Inf
#> [2,] -Inf 5.25  6.5 7.75  Inf
quantile(qp, lower = 0)
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,]    0 1.25  2.5 3.75  Inf
#> [2,]    0 5.25  6.5 7.75  Inf
quantile(qp, probs = 0.5)
#>      [,1]
#> [1,]  2.5
#> [2,]  6.5
quantile(qp, probs = 1:9 / 10)
#>           [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]     [,9]
#> [1,] 0.2477407    1  1.5    2  2.5    3  3.5    4 4.752259
#> [2,] 4.2477407    5  5.5    6  6.5    7  7.5    8 8.752259
```
