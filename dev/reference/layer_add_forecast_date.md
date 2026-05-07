# Post-processing step to add the forecast date

Post-processing step to add the forecast date

## Usage

``` r
layer_add_forecast_date(
  frosting,
  forecast_date = NULL,
  id = rand_id("add_forecast_date")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor

- forecast_date:

  The forecast date to add as a column to the `epi_df`. For most cases,
  this should be specified in the form "yyyy-mm-dd". Note that when the
  forecast date is left unspecified, it is set to one of two values. If
  there is a `step_adjust_latency` step present, it uses the
  `forecast_date` as set in that function. Otherwise, it uses the
  maximum `time_value` across the data used for pre-processing, fitting
  the model, and post-processing.

- id:

  a random id string

## Value

an updated `frosting` postprocessor

## Details

To use this function, either specify a forecast date or leave the
forecast date unspecifed here. In the latter case, the forecast date
will be set as the maximum time value from the data used in
pre-processing, fitting the model, and post-processing. In any case,
when the forecast date is less than the maximum `as_of` value (from the
data used pre-processing, model fitting, and post-processing), an
appropriate warning will be thrown.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()
wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
latest <- jhu %>%
  filter(time_value >= max(time_value) - 14)

# Don't specify `forecast_date` (by default, this should be last date in latest)
f <- frosting() %>%
  layer_predict() %>%
  layer_naomit(.pred)
wf0 <- wf %>% add_frosting(f)
p0 <- predict(wf0, latest)
p0
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

# Specify a `forecast_date` that is greater than or equal to `as_of` date
f <- frosting() %>%
  layer_predict() %>%
  layer_add_forecast_date(forecast_date = "2022-05-31") %>%
  layer_naomit(.pred)
wf1 <- wf %>% add_frosting(f)

p1 <- predict(wf1, latest)
p1
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred forecast_date
#>   <chr>     <date>     <dbl> <date>       
#> 1 ak        2021-12-31 0.245 2022-05-31   
#> 2 ca        2021-12-31 0.312 2022-05-31   
#> 3 ny        2021-12-31 0.295 2022-05-31   

# Specify a `forecast_date` that is less than `as_of` date
f2 <- frosting() %>%
  layer_predict() %>%
  layer_add_forecast_date(forecast_date = "2021-12-31") %>%
  layer_naomit(.pred)
wf2 <- wf %>% add_frosting(f2)

p2 <- predict(wf2, latest)
p2
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred forecast_date
#>   <chr>     <date>     <dbl> <date>       
#> 1 ak        2021-12-31 0.245 2021-12-31   
#> 2 ca        2021-12-31 0.312 2021-12-31   
#> 3 ny        2021-12-31 0.295 2021-12-31   

# Do not specify a forecast_date
f3 <- frosting() %>%
  layer_predict() %>%
  layer_add_forecast_date() %>%
  layer_naomit(.pred)
wf3 <- wf %>% add_frosting(f3)

p3 <- predict(wf3, latest)
p3
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred forecast_date
#>   <chr>     <date>     <dbl> <date>       
#> 1 ak        2021-12-31 0.245 2021-12-31   
#> 2 ca        2021-12-31 0.312 2021-12-31   
#> 3 ny        2021-12-31 0.295 2021-12-31   
```
