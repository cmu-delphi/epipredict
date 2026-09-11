# Climatological forecaster argument constructor

Climatological forecaster argument constructor

## Usage

``` r
climate_args_list(
  forecast_date = NULL,
  forecast_horizon = 0:4,
  time_type = c("epiweek", "week", "month", "day"),
  center_method = c("median", "mean"),
  window_size = 3L,
  quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  symmetrize = FALSE,
  nonneg = TRUE,
  quantile_by_key = character(0L),
  ...
)
```

## Arguments

- forecast_date:

  Date. The date from which the forecast is occurring. The default
  `NULL` will determine this automatically from either

  1.  the maximum time value for which there's data if there is no
      latency adjustment (the default case), or

  2.  the `as_of` date of `epi_data` if `adjust_latency` is non-`NULL`.

- forecast_horizon:

  Vector of integers giving the number of time steps, in units of the
  `time_type`, from the `reference_date` for which predictions should be
  produced.

- time_type:

  The duration over which time aggregation should be performed.

- center_method:

  The measure of center to be calculated over the time window.

- window_size:

  Scalar integer. How many time units on each side should be included.
  For example, if `window_size = 3` and `time_type = "day"`, then on
  each day in the data, the center will be calculated using 3 days
  before and three days after. So, in this case, it operates like a
  weekly rolling average, centered at each day.

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
  [`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/layer_residual_quantiles.md)
  for more information. The default, `character(0)` performs no
  grouping. This argument only applies when residual quantiles are used.
  It is not applicable with `trainer = quantile_reg()`, for example.

- ...:

  Further arguments passed to or from other methods (not currently
  used).

## Value

A list containing updated parameter choices with class `climate_alist`.

## See also

[`climatological_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/climatological_forecaster.md),
[`step_climate()`](https://cmu-delphi.github.io/epipredict/reference/step_climate.md)

## Examples

``` r
climate_args_list()
#> • forecast_date : "NULL"
#> • forecast_horizon : 0, 1, 2, 3, and 4
#> • time_type : "epiweek"
#> • center_method : "median"
#> • window_size : 3
#> • quantile_levels : 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, and 0.95
#> • symmetrize : FALSE
#> • nonneg : TRUE
#> • quantile_by_key : "_empty_"
climate_args_list(
  forecast_horizon = 0:10,
  quantile_levels = c(.01, .025, 1:19 / 20, .975, .99)
)
#> • forecast_date : "NULL"
#> • forecast_horizon : 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, and 10
#> • time_type : "epiweek"
#> • center_method : "median"
#> • window_size : 3
#> • quantile_levels : 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
#>   0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, …, 0.975, and 0.99
#> • symmetrize : FALSE
#> • nonneg : TRUE
#> • quantile_by_key : "_empty_"
```
