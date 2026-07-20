# Prediction layer for post-processing

Implements prediction on a fitted `epi_workflow`. One may want different
types of prediction, and to potentially apply this after some amount of
post-processing. This would typically be the first layer in a `frosting`
postprocessor.

## Usage

``` r
layer_predict(
  frosting,
  type = NULL,
  opts = list(),
  ...,
  id = rand_id("predict_default")
)
```

## Arguments

- frosting:

  a frosting object

- type:

  A single character value or `NULL`. Possible values are `"numeric"`,
  `"class"`, `"prob"`, `"conf_int"`, `"pred_int"`, `"quantile"`,
  `"time"`, `"hazard"`, `"survival"`, or `"raw"`. When `NULL`,
  [`predict()`](https://rdrr.io/r/stats/predict.html) will choose an
  appropriate value based on the model's mode.

- opts:

  A list of optional arguments to the underlying predict function that
  will be used when `type = "raw"`. The list should not include options
  for the model object or the new data being predicted.

- ...:

  Additional `parsnip`-related options, depending on the value of
  `type`. Arguments to the underlying model's prediction function cannot
  be passed here (use the `opts` argument instead). Possible arguments
  are:

  - `interval`: for `type` equal to `"survival"` or `"quantile"`, should
    interval estimates be added, if available? Options are `"none"` and
    `"confidence"`.

  - `level`: for `type` equal to `"conf_int"`, `"pred_int"`, or
    `"survival"`, this is the parameter for the tail area of the
    intervals (e.g. confidence level for confidence intervals). Default
    value is `0.95`.

  - `std_error`: for `type` equal to `"conf_int"` or `"pred_int"`, add
    the standard error of fit or prediction (on the scale of the linear
    predictors). Default value is `FALSE`.

  - `quantile`: for `type` equal to `quantile`, the quantiles of the
    distribution. Default is `(1:9)/10`.

  - `eval_time`: for `type` equal to `"survival"` or `"hazard"`, the
    time points at which the survival probability or hazard is
    estimated.

- id:

  a string identifying the layer

## Value

An updated `frosting` object

## See also

[`parsnip::predict.model_fit()`](https://parsnip.tidymodels.org/reference/predict.model_fit.html)

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
latest <- jhu %>% filter(time_value >= max(time_value) - 14)

# Predict layer alone
f <- frosting() %>% layer_predict()
wf1 <- wf %>% add_frosting(f)

p1 <- predict(wf1, latest)
p1
#> An `epi_df` object, 3 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 3 × 3
#>   geo_value time_value .pred
#>   <chr>     <date>     <dbl>
#> 1 ak        2021-12-31 0.245
#> 2 ca        2021-12-31 0.312
#> 3 ny        2021-12-31 0.295

# Prediction with interval
f <- frosting() %>% layer_predict(type = "pred_int")
wf2 <- wf %>% add_frosting(f)

p2 <- predict(wf2, latest)
p2
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred_lower .pred_upper
#>   <chr>     <date>           <dbl>       <dbl>
#> 1 ak        2021-12-31      -0.366       0.857
#> 2 ca        2021-12-31      -0.285       0.909
#> 3 ny        2021-12-31      -0.302       0.891
```
