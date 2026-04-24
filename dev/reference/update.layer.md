# Update post-processing `layer`

This `layer` method for
[`update()`](https://rdrr.io/r/stats/update.html) takes named arguments
as `...` whose values will replace the elements of the same name in the
actual post-processing layer. Analogous to `update.step()` from the
`recipes` package.

## Usage

``` r
# S3 method for class 'layer'
update(object, ...)
```

## Arguments

- object:

  A post-processing `layer`.

- ...:

  Key-value pairs where the keys match up with names of elements in the
  layer, and the values are the new values to update the layer with.

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
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred forecast_date
#>   <chr>     <date>     <dbl> <date>       
#> 1 ak        2021-12-31 0.245 2022-05-31   
#> 2 ca        2021-12-31 0.312 2022-05-31   
#> 3 ny        2021-12-31 0.295 2022-05-31   

# Update forecast date
f$layers[[2]] <- update(f$layers[[2]], forecast_date = "2021-06-01")

# Need to still update workflow if only update a layer in frosting
wf2 <- wf %>% add_frosting(f)
wf2$post # Check that wf1 has update
#> $actions
#> $actions$frosting
#> $frosting
#> 
#> ── Frosting ────────────────────────────────────────────────────────────────────
#> 
#> ── Layers 
#> 1. Creating predictions: "<calculated>"
#> 2. Adding forecast date: "2021-06-01"
#> 3. Removing na predictions from: .pred
#> 
#> attr(,"class")
#> [1] "action_post" "action"     
#> 
#> 
#> $fit
#> NULL
#> 
#> attr(,"class")
#> [1] "stage_post" "stage"     
p1 <- predict(wf2, latest)
p1
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value .pred forecast_date
#>   <chr>     <date>     <dbl> <date>       
#> 1 ak        2021-12-31 0.245 2021-06-01   
#> 2 ca        2021-12-31 0.312 2021-06-01   
#> 3 ny        2021-12-31 0.295 2021-06-01   
```
