# CDC baseline forecaster argument constructor

Constructs a list of arguments for
[`cdc_baseline_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/cdc_baseline_forecaster.md).

## Usage

``` r
cdc_baseline_args_list(
  data_frequency = "1 week",
  aheads = 1:5,
  n_training = Inf,
  forecast_date = NULL,
  quantile_levels = c(0.01, 0.025, 1:19/20, 0.975, 0.99),
  nsims = 100000L,
  symmetrize = TRUE,
  nonneg = TRUE,
  quantile_by_key = "geo_value",
  ...
)
```

## Arguments

- data_frequency:

  Integer or string. This describes the frequency of the input `epi_df`.
  For typical FluSight forecasts, this would be `"1 week"`. Allowable
  arguments are integers (taken to mean numbers of days) or a string
  like `"7 days"` or `"2 weeks"`. Currently, all other periods (other
  than days or weeks) result in an error.

- aheads:

  Integer vector. Unlike
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_forecaster.md),
  this doesn't have any effect on the predicted values. Predictions are
  always the most recent observation. This determines the set of
  prediction horizons for
  [`layer_cdc_flatline_quantiles()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_cdc_flatline_quantiles.md)`. It interacts with the `data_frequency`argument. So, for example, if the data is daily and you want forecasts for 1:4 days ahead, then you would use`1:4`. However, if you want one-week predictions, you would set this as `c(7,
  14, 21, 28)`. But if `data_frequency`is`"1
  week"`, then you would set it as `1:4\`.

- n_training:

  Integer. An upper limit for the number of rows per key that are used
  for training (in the time unit of the `epi_df`).

- forecast_date:

  Date. The date from which the forecast is occurring. The default
  `NULL` will determine this automatically from either

  1.  the maximum time value for which there's data if there is no
      latency adjustment (the default case), or

  2.  the `as_of` date of `epi_data` if `adjust_latency` is non-`NULL`.

- quantile_levels:

  Vector or `NULL`. A vector of probabilities to produce prediction
  intervals. These are created by computing the quantiles of training
  residuals. A `NULL` value will result in point forecasts only.

- nsims:

  Positive integer. The number of draws from the empirical CDF. These
  samples are spaced evenly on the (0, 1) scale, F_X(x) resulting in
  linear interpolation on the X scale. This is achieved with
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) Type 7
  (the default for that function).

- symmetrize:

  Logical. The default `TRUE` calculates symmetric prediction intervals.
  This argument only applies when residual quantiles are used. It is not
  applicable with `trainer = quantile_reg()`, for example. Typically,
  one would only want non-symmetric quantiles when increasing
  trajectories are quite different from decreasing ones, such as a
  strictly postive variable near zero.

- nonneg:

  Logical. Force all predictive intervals be non-negative. Because
  non-negativity is forced *before* propagating forward, this has
  slightly different behaviour than would occur if using
  [`layer_threshold()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_threshold.md).

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

A list containing updated parameter choices with class `cdc_flat_fcast`.

## Examples

``` r
cdc_baseline_args_list()
#> • data_frequency : 7
#> • aheads : 1, 2, 3, 4, and 5
#> • n_training : Inf
#> • forecast_date : "NULL"
#> • quantile_levels : 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
#>   0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, …, 0.975, and 0.99
#> • nsims : 100000
#> • symmetrize : TRUE
#> • nonneg : TRUE
#> • quantile_by_key : "geo_value"
cdc_baseline_args_list(symmetrize = FALSE)
#> • data_frequency : 7
#> • aheads : 1, 2, 3, 4, and 5
#> • n_training : Inf
#> • forecast_date : "NULL"
#> • quantile_levels : 0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4,
#>   0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, …, 0.975, and 0.99
#> • nsims : 100000
#> • symmetrize : FALSE
#> • nonneg : TRUE
#> • quantile_by_key : "geo_value"
cdc_baseline_args_list(quantile_levels = c(.1, .3, .7, .9), n_training = 120)
#> • data_frequency : 7
#> • aheads : 1, 2, 3, 4, and 5
#> • n_training : 120
#> • forecast_date : "NULL"
#> • quantile_levels : 0.1, 0.3, 0.7, and 0.9
#> • nsims : 100000
#> • symmetrize : TRUE
#> • nonneg : TRUE
#> • quantile_by_key : "geo_value"
```
