# Lower and upper thresholds for predicted values

This post-processing step is used to set prediction values that are
smaller than the lower threshold or higher than the upper threshold
equal to the threshold values.

## Usage

``` r
layer_threshold(
  frosting,
  ...,
  lower = 0,
  upper = Inf,
  id = rand_id("threshold")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.
  Typical usage is `.pred` to threshold predictions to a range (say,
  nonnegative).

- lower:

  Lower threshold for the prediction values. That is, any predictions
  that are less than this lower bound are set to it. Default value is
  `0`.

- upper:

  Upper threshold for the prediction values. That is, any predictions
  that are greater than this upper bound are set to it. Default value is
  `Inf`.

- id:

  a random id string

## Value

an updated `frosting` postprocessor

## Details

Making case count predictions strictly positive is a typical example
usage. It must be called after there is a column containing quantiles.
This means at earliest it can be called after
[`layer_predict()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_predict.md)
for distributional models, or after
[`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_residual_quantiles.md)
for point prediction models. Typical best practice will use
`starts_with(".pred")` as the variables to threshold.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value < "2021-03-08", geo_value %in% c("ak", "ca", "ar"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()
wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)

f <- frosting() %>%
  layer_predict() %>%
  layer_threshold(starts_with(".pred"), lower = 0.180, upper = 0.310)
wf <- wf %>% add_frosting(f)
p <- forecast(wf)
p
#> An `epi_df` object, 3 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 733 days
#> 
#> # A tibble: 3 × 3
#>   geo_value time_value .pred
#>   <chr>     <date>     <dbl>
#> 1 ak        2021-03-07  0.18
#> 2 ar        2021-03-07  0.18
#> 3 ca        2021-03-07  0.31
```
