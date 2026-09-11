# A distribution parameterized by a set of quantiles

**\[deprecated\]**

## Usage

``` r
dist_quantiles(values, quantile_levels)
```

## Arguments

- values:

  A vector (or list of vectors) of values.

- quantile_levels:

  A vector (or list of vectors) of probabilities corresponding to
  `values`.

  When creating multiple sets of `values`/`quantile_levels` resulting in
  different distributions, the sizes must match. See the examples below.

## Value

A vector of class `"distribution"`.

## Details

This function is deprecated. The recommended alternative is
[`hardhat::quantile_pred()`](https://hardhat.tidymodels.org/reference/quantile_pred.html).
