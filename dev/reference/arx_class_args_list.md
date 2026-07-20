# ARX classifier argument constructor

Constructs a list of arguments for
[`arx_classifier()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_classifier.md).

## Usage

``` r
arx_class_args_list(
  lags = c(0L, 7L, 14L),
  ahead = 7L,
  n_training = Inf,
  forecast_date = NULL,
  target_date = NULL,
  adjust_latency = c("none", "extend_ahead", "extend_lags", "locf"),
  warn_latency = TRUE,
  outcome_transform = c("growth_rate", "lag_difference"),
  breaks = 0.25,
  horizon = 7L,
  method = c("rel_change", "linear_reg"),
  log_scale = FALSE,
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

- outcome_transform:

  Scalar character. Whether the outcome should be created using growth
  rates (as the predictors are) or lagged differences. The second case
  is closer to the requirements for the [2022-23 CDC Flusight
  Hospitalization Experimental
  Target](https://github.com/cdcepi/Flusight-forecast-data/blob/745511c436923e1dc201dea0f4181f21a8217b52/data-experimental/README.md).
  See the
  [`vignette("epipredict")`](https://cmu-delphi.github.io/epipredict/dev/articles/epipredict.md)
  for more details. Selecting `"growth_rate"` (the default) uses
  [`epiprocess::growth_rate()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate.html)
  to create the outcome using some of the additional arguments below.
  Choosing `"lag_difference"` instead simply uses the change from the
  value at the selected `horizon`.

- breaks:

  Vector. A vector of breaks to turn real-valued growth rates into
  discrete classes. The default gives binary upswing classification as
  in [McDonald, Bien, Green, Hu, et
  al.](https://doi.org/10.1073/pnas.2111453118). This coincides with the
  default `trainer = parsnip::logistic_reg()` argument in
  [`arx_classifier()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_classifier.md).
  However, multiclass classification is also supported (e.g. with
  `breaks = c(-.2, .25)`) provided that
  `trainer = parsnip::multinom_reg()` (or another multiclass trainer) is
  used as well. These will be sliently expanded to cover the entire real
  line (so the default will become `breaks = c(-Inf, .25, Inf)`) before
  being used to discretize the response. This is different than the
  behaviour in
  [`recipes::step_cut()`](https://recipes.tidymodels.org/reference/step_cut.html)
  which creates classes that only cover the range of the training data.

- horizon:

  Scalar integer. This is passed to the `h` argument of
  [`epiprocess::growth_rate()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate.html).
  It determines the amount of data used to calculate the growth rate.

- method:

  Character. Options available for growth rate calculation.

- log_scale:

  Scalar logical. Whether to compute growth rates on the log scale.

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

A list containing updated parameter choices with class `arx_clist`.

## Examples

``` r
arx_class_args_list()
#> • lags : 0, 7, and 14
#> • ahead : 7
#> • n_training : Inf
#> • breaks : -Inf, 0.25, and Inf
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • adjust_latency : "none"
#> • outcome_transform : "growth_rate"
#> • max_lags : 14
#> • horizon : 7
#> • method : "rel_change"
#> • log_scale : FALSE
#> • check_enough_data_n : "NULL"
#> • check_enough_data_epi_keys : "NULL"

# 3-class classsification,
# also needs arx_classifier(trainer = parsnip::multinom_reg())
arx_class_args_list(breaks = c(-.2, .25))
#> • lags : 0, 7, and 14
#> • ahead : 7
#> • n_training : Inf
#> • breaks : -Inf, -0.2, 0.25, and Inf
#> • forecast_date : "NULL"
#> • target_date : "NULL"
#> • adjust_latency : "none"
#> • outcome_transform : "growth_rate"
#> • max_lags : 14
#> • horizon : 7
#> • method : "rel_change"
#> • log_scale : FALSE
#> • check_enough_data_n : "NULL"
#> • check_enough_data_epi_keys : "NULL"
```
