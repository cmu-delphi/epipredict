# Extrapolate the quantiles to new quantile levels

This both interpolates between quantile levels already defined in `x`
and extrapolates quantiles outside their bounds. The interpolation
method is determined by the `quantile` argument `middle`, which can be
either `"cubic"` for a (Hyman) cubic spline interpolation, or `"linear"`
for simple linear interpolation.

## Usage

``` r
extrapolate_quantiles(x, probs, replace_na = TRUE, ...)
```

## Arguments

- x:

  A vector of class `quantile_pred`.

- probs:

  a vector of probabilities at which to calculate quantiles

- replace_na:

  logical. If `x` contains `NA`'s, these are imputed if possible (if
  `TRUE`) or retained (if `FALSE`).

- ...:

  additional arguments passed on to the `quantile` method

## Value

a `quantile_pred` vector. Each element of `x` will now have a superset
of the original `quantile_values` (the union of those and `probs`).

## Details

There is only one extrapolation method for values greater than the
largest available quantile level or smaller than the smallest available
quantile level. It assumes a roughly exponential tail, whose decay rate
and offset is derived from the slope of the two most extreme quantile
levels on a logistic scale. See the internal function
`tail_extrapolate()` for the exact implementation.

This function takes a `quantile_pred` vector and returns the same type
of object, expanded to include *additional* quantiles computed at
`probs`. If you want behaviour more similar to
[`stats::quantile()`](https://rdrr.io/r/stats/quantile.html), then
`quantile(x,...)` may be more appropriate.

## Examples

``` r
dstn <- quantile_pred(rbind(1:4, 8:11), c(.2, .4, .6, .8))
# extra quantiles are appended
as_tibble(extrapolate_quantiles(dstn, probs = c(0.25, 0.5, 0.75)))
#> # A tibble: 14 × 3
#>    .pred_quantile .quantile_levels  .row
#>             <dbl>            <dbl> <int>
#>  1           1                0.2      1
#>  2           1.25             0.25     1
#>  3           2                0.4      1
#>  4           2.5              0.5      1
#>  5           3                0.6      1
#>  6           3.75             0.75     1
#>  7           4                0.8      1
#>  8           8                0.2      2
#>  9           8.25             0.25     2
#> 10           9                0.4      2
#> 11           9.5              0.5      2
#> 12          10                0.6      2
#> 13          10.8              0.75     2
#> 14          11                0.8      2

extrapolate_quantiles(dstn, probs = c(0.0001, 0.25, 0.5, 0.75, 0.99999))
#> <quantiles[2]>
#> [1] [2.5] [9.5]
#> # Quantile levels: 0.0001 0.2000 0.2500 0.4000 0.5000 0.6000 0.7500 0.8000 1.0000 
```
