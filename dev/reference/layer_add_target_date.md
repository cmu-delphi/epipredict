# Post-processing step to add the target date

Post-processing step to add the target date

## Usage

``` r
layer_add_target_date(
  frosting,
  target_date = NULL,
  id = rand_id("add_target_date")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor

- target_date:

  The target date to add as a column to the `epi_df`. If there's a
  forecast date specified upstream (either in a `step_adjust_latency` or
  in a `layer_forecast_date`), then it is the forecast date plus `ahead`
  (from `step_epi_ahead` in the `epi_recipe`). Otherwise, it is the
  maximum `time_value` (from the data used in pre-processing, fitting
  the model, and post-processing) plus `ahead`, where `ahead` has been
  specified in preprocessing. The user may override these by specifying
  a target date of their own (of the form "yyyy-mm-dd").

- id:

  a random id string

## Value

an updated `frosting` postprocessor

## Details

By default, this function assumes that a value for `ahead` has been
specified in a preprocessing step (most likely in `step_epi_ahead`).
Then, `ahead` is added to the `forecast_date` in the test data to get
the target date. `forecast_date` itself can be set in 3 ways:

1.  The default `forecast_date` is simply the maximum `time_value` over
    every dataset used (prep, training, and prediction).

2.  if `step_adjust_latency` is present, it will typically use the
    training `epi_df`'s `as_of`

3.  `layer_add_forecast_date`, which inherits from 2 if not manually
    specifed

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)

# Use ahead + forecast date
f <- frosting() %>%
  layer_predict() %>%
  layer_add_forecast_date(forecast_date = as.Date("2022-05-31")) %>%
  layer_add_target_date() %>%
  layer_naomit(.pred)
wf1 <- wf %>% add_frosting(f)

p <- forecast(wf1)
p
#> An `epi_df` object, 3 x 5 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 434 days
#> 
#> # A tibble: 3 × 5
#>   geo_value time_value .pred forecast_date target_date
#>   <chr>     <date>     <dbl> <date>        <date>     
#> 1 ak        2021-12-31 0.245 2022-05-31    2022-06-07 
#> 2 ca        2021-12-31 0.312 2022-05-31    2022-06-07 
#> 3 ny        2021-12-31 0.295 2022-05-31    2022-06-07 

# Use ahead + forecast_date from adjust_latency
# setting the `as_of` to something realistic
attributes(jhu)$metadata$as_of <- max(jhu$time_value) + 3
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_adjust_latency(method = "extend_ahead") %>%
  step_epi_naomit()
#> Warning: If `method` is "extend_ahead", then the previous `step_epi_ahead` won't be
#> modified.
f2 <- frosting() %>%
  layer_predict() %>%
  layer_add_target_date() %>%
  layer_naomit(.pred)
wf2 <- wf %>% add_frosting(f2)

p2 <- forecast(wf2)
p2
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred target_date
#>   <chr>     <date>     <dbl> <date>     
#> 1 ak        2021-12-31 0.245 2022-01-07 
#> 2 ca        2021-12-31 0.312 2022-01-07 
#> 3 ny        2021-12-31 0.295 2022-01-07 

# Use ahead + max time value from pre, fit, post
# which is the same if include `layer_add_forecast_date()`
f3 <- frosting() %>%
  layer_predict() %>%
  layer_add_target_date() %>%
  layer_naomit(.pred)
wf3 <- wf %>% add_frosting(f3)

p3 <- forecast(wf2)
p2
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred target_date
#>   <chr>     <date>     <dbl> <date>     
#> 1 ak        2021-12-31 0.245 2022-01-07 
#> 2 ca        2021-12-31 0.312 2022-01-07 
#> 3 ny        2021-12-31 0.295 2022-01-07 

# Specify own target date
f4 <- frosting() %>%
  layer_predict() %>%
  layer_add_target_date(target_date = "2022-01-08") %>%
  layer_naomit(.pred)
wf4 <- wf %>% add_frosting(f4)

p4 <- forecast(wf4)
p4
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred target_date
#>   <chr>     <date>     <dbl> <date>     
#> 1 ak        2021-12-31 0.245 2022-01-08 
#> 2 ca        2021-12-31 0.312 2022-01-08 
#> 3 ny        2021-12-31 0.295 2022-01-08 
```
