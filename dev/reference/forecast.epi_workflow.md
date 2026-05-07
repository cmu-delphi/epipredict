# Produce a forecast from an epi workflow and it's training data

`forecast.epi_workflow` predicts by restricting the training data to the
latest available data, and predicting on that. It binds together
[`get_test_data()`](https://cmu-delphi.github.io/epipredict/dev/reference/get_test_data.md)
and [`predict()`](https://rdrr.io/r/stats/predict.html).

## Usage

``` r
# S3 method for class 'epi_workflow'
forecast(object, ...)
```

## Arguments

- object:

  An epi workflow.

- ...:

  Not used.

## Value

A forecast tibble.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-08-01")

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

epi_workflow(r, parsnip::linear_reg()) %>%
  fit(jhu) %>%
  forecast()
#> An `epi_df` object, 56 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 56 × 3
#>    geo_value time_value .pred
#>    <chr>     <date>     <dbl>
#>  1 ak        2021-12-31 0.550
#>  2 al        2021-12-31 0.290
#>  3 ar        2021-12-31 0.504
#>  4 as        2021-12-31 0.148
#>  5 az        2021-12-31 0.724
#>  6 ca        2021-12-31 0.258
#>  7 co        2021-12-31 0.454
#>  8 ct        2021-12-31 0.333
#>  9 dc        2021-12-31 0.204
#> 10 de        2021-12-31 0.402
#> # ℹ 46 more rows
```
