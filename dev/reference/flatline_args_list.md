# Flatline forecaster argument constructor

Constructs a list of arguments for
[`flatline_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/flatline_forecaster.md).

## Usage

``` r
flatline_args_list(
  ahead = 7L,
  n_training = Inf,
  forecast_date = NULL,
  target_date = NULL,
  quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  symmetrize = TRUE,
  nonneg = TRUE,
  quantile_by_key = character(0L),
  ...
)
```

## Arguments

- ahead:

  Integer. Unlike
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_forecaster.md),
  this doesn't have any effect on the predicted values. Predictions are
  always the most recent observation. However, this *does* impact the
  residuals stored in the object. Residuals are calculated based on this
  number to mimic how badly you would have done. So for example,
  `ahead = 7` will create residuals by comparing values 7 days apart.

- n_training:

  Integer. An upper limit for the number of rows per key that are used
  for training (in the time unit of the `epi_df`).

- forecast_date:

  Date. The date from which the forecast is occurring. The default
  `NULL` will determine this automatically from either

  1.  the maximum time value for which there's data if there is no
      latency adjustment (the default case), or

  2.  the `as_of` date of `epi_data` if `adjust_latency` is non-`NULL`.

- target_date:

  Date. The date that is being forecast. The default `NULL` will
  determine this automatically as `forecast_date + ahead`.

- quantile_levels:

  Vector or `NULL`. A vector of probabilities to produce prediction
  intervals. These are created by computing the quantiles of training
  residuals. A `NULL` value will result in point forecasts only.

- symmetrize:

  Logical. The default `TRUE` calculates symmetric prediction intervals.
  This argument only applies when residual quantiles are used. It is not
  applicable with `trainer = quantile_reg()`, for example. Typically,
  one would only want non-symmetric quantiles when increasing
  trajectories are quite different from decreasing ones, such as a
  strictly postive variable near zero.

- nonneg:

  Logical. The default `TRUE` enforces nonnegative predictions by
  hard-thresholding at 0.

- quantile_by_key:

  Character vector. Groups residuals by listed keys before calculating
  residual quantiles. See the `by_key` argument to
  [`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_residual_quantiles.md)
  for more information. The default, `character(0)` performs no
  grouping. This argument only applies when residual quantiles are used.
  It is not applicable with `trainer = quantile_reg()`, for example.

- ...:

  Space to handle future expansions (unused).

## Value

A list containing updated parameter choices with class `flatline_alist`.

## Examples

``` r
flatline_args_list()
#> • ahead : 7
#> • n_training : Inf
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • quantile_levels : 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, and 0.95
#> • symmetrize : TRUE
#> • nonneg : TRUE
#> • quantile_by_key : "_empty_"
flatline_args_list(symmetrize = FALSE)
#> • ahead : 7
#> • n_training : Inf
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • quantile_levels : 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, and 0.95
#> • symmetrize : FALSE
#> • nonneg : TRUE
#> • quantile_by_key : "_empty_"
flatline_args_list(quantile_levels = c(.1, .3, .7, .9), n_training = 120)
#> • ahead : 7
#> • n_training : 120
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • quantile_levels : 0.1, 0.3, 0.7, and 0.9
#> • symmetrize : TRUE
#> • nonneg : TRUE
#> • quantile_by_key : "_empty_"
```
