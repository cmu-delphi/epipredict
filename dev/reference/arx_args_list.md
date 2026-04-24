# ARX forecaster argument constructor

Constructs a list of arguments for
[`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_forecaster.md).

## Usage

``` r
arx_args_list(
  lags = c(0L, 7L, 14L),
  ahead = 7L,
  n_training = Inf,
  forecast_date = NULL,
  target_date = NULL,
  adjust_latency = c("none", "extend_ahead", "extend_lags", "locf"),
  warn_latency = TRUE,
  quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  symmetrize = TRUE,
  nonneg = TRUE,
  quantile_by_key = character(0L),
  check_enough_data_n = NULL,
  check_enough_data_epi_keys = NULL,
  ...
)
```

## Arguments

- lags:

  Vector or List. Positive integers enumerating lags to use in
  autoregressive-type models (in days). By default, an unnamed list of
  lags will be set to correspond to the order of the predictors.

- ahead:

  Integer. Number of time steps ahead (in days) of the forecast date for
  which forecasts should be produced.

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

- adjust_latency:

  Character. One of the `method`s of
  [`step_adjust_latency()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_adjust_latency.md),
  or `"none"` (in which case there is no adjustment). If the
  `forecast_date` is after the last day of data, this determines how to
  shift the model to account for this difference. The options are:

  - `"none"` the default, assumes the `forecast_date` is the last day of
    data

  - `"extend_ahead"`: increase the `ahead` by the latency so it's
    relative to the last day of data. For example, if the last day of
    data was 3 days ago, the ahead becomes `ahead+3`.

  - `"extend_lags"`: increase the lags so they're relative to the actual
    forecast date. For example, if the lags are `c(0, 7, 14)` and the
    last day of data was 3 days ago, the lags become `c(3, 10, 17)`.

- warn_latency:

  by default, `step_adjust_latency` warns the user if the latency is
  large. If this is `FALSE`, that warning is turned off.

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

- check_enough_data_n:

  Integer. A lower limit for the number of rows per epi_key that are
  required for training. If `NULL`, this check is ignored.

- check_enough_data_epi_keys:

  Character vector. A character vector of column names on which to group
  the data and check threshold within each group. Useful if training per
  group (for example, per geo_value).

- ...:

  Space to handle future expansions (unused).

## Value

A list containing updated parameter choices with class `arx_flist`.

## See also

[`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_forecaster.md)

## Examples

``` r
arx_args_list()
#> • lags : 0, 7, and 14
#> • ahead : 7
#> • n_training : Inf
#> • quantile_levels : 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, and 0.95
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • adjust_latency : "none"
#> • warn_latency : TRUE
#> • symmetrize : TRUE
#> • nonneg : TRUE
#> • max_lags : 14
#> • quantile_by_key : "_empty_"
#> • check_enough_data_n : "NULL"
#> • check_enough_data_epi_keys : "NULL"
arx_args_list(symmetrize = FALSE)
#> • lags : 0, 7, and 14
#> • ahead : 7
#> • n_training : Inf
#> • quantile_levels : 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, and 0.95
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • adjust_latency : "none"
#> • warn_latency : TRUE
#> • symmetrize : FALSE
#> • nonneg : TRUE
#> • max_lags : 14
#> • quantile_by_key : "_empty_"
#> • check_enough_data_n : "NULL"
#> • check_enough_data_epi_keys : "NULL"
arx_args_list(quantile_levels = c(.1, .3, .7, .9), n_training = 120)
#> • lags : 0, 7, and 14
#> • ahead : 7
#> • n_training : 120
#> • quantile_levels : 0.1, 0.3, 0.7, and 0.9
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • adjust_latency : "none"
#> • warn_latency : TRUE
#> • symmetrize : TRUE
#> • nonneg : TRUE
#> • max_lags : 14
#> • quantile_by_key : "_empty_"
#> • check_enough_data_n : "NULL"
#> • check_enough_data_epi_keys : "NULL"
```
