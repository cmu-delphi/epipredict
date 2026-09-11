# Get test data for prediction based on longest lag period

If [`predict()`](https://rdrr.io/r/stats/predict.html) is given the full
training dataset, it will produce a prediction for every `time_value`
which has enough data. For most cases, this generates predictions for
`time_values` where the `outcome` has already been observed.
`get_test_data()` is designed to restrict the given dataset to the
minimum amount needed to produce a forecast on the `forecast_date` for
future data, rather than a prediction on past `time_value`s. Primarily
this is based on the longest lag period in the recipe.

## Usage

``` r
get_test_data(recipe, x)
```

## Arguments

- recipe:

  A recipe object.

- x:

  An epi_df. The typical usage is to pass the same data as that used for
  fitting the recipe.

## Value

An object of the same type as `x` with columns `geo_value`,
`time_value`, any additional keys, as well other variables in the
original dataset.

## Details

The minimum required (recent) data to produce a forecast is equal to the
maximum lag requested (on any predictor) plus the longest horizon used
if growth rate calculations are requested by the recipe. This is
calculated internally.

## Examples

``` r
# create recipe
rec <- epi_recipe(covid_case_death_rates) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14))
get_test_data(recipe = rec, x = covid_case_death_rates)
#> An `epi_df` object, 840 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 434 days
#> 
#> # A tibble: 840 × 4
#>    geo_value time_value case_rate death_rate
#>    <chr>     <date>         <dbl>      <dbl>
#>  1 ak        2021-12-17      23.1      1.19 
#>  2 al        2021-12-17      15.6      0.290
#>  3 ar        2021-12-17      23.4      0.467
#>  4 as        2021-12-17       0        0    
#>  5 az        2021-12-17      41.2      1.04 
#>  6 ca        2021-12-17      16.8      0.158
#>  7 co        2021-12-17      31.8      0.371
#>  8 ct        2021-12-17      64.8      0.120
#>  9 dc        2021-12-17      50.4      0.140
#> 10 de        2021-12-17      67.9      0.333
#> # ℹ 830 more rows
```
