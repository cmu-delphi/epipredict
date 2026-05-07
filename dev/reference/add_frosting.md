# Add/remove/update the `frosting` of an `epi_workflow`

Add/remove/update the `frosting` of an `epi_workflow`

## Usage

``` r
add_frosting(x, frosting, ...)

remove_frosting(x)

update_frosting(x, frosting, ...)
```

## Arguments

- x:

  A workflow

- frosting:

  A frosting object created using
  [`frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/frosting.md).

- ...:

  Not used.

## Value

`x`, updated with a new frosting postprocessor

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7)

wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
latest <- jhu %>%
  filter(time_value >= max(time_value) - 14)

# Add frosting to a workflow and predict
f <- frosting() %>%
  layer_predict() %>%
  layer_naomit(.pred)
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

# Update frosting in a workflow and predict
f2 <- frosting() %>% layer_predict()
wf2 <- wf1 %>% update_frosting(f2)
p2 <- predict(wf2, latest)
p2
#> An `epi_df` object, 108 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 108 × 3
#>    geo_value time_value .pred
#>    <chr>     <date>     <dbl>
#>  1 ak        2021-12-10    NA
#>  2 ca        2021-12-10    NA
#>  3 ny        2021-12-10    NA
#>  4 ak        2021-12-11    NA
#>  5 ca        2021-12-11    NA
#>  6 ny        2021-12-11    NA
#>  7 ak        2021-12-12    NA
#>  8 ca        2021-12-12    NA
#>  9 ny        2021-12-12    NA
#> 10 ak        2021-12-13    NA
#> # ℹ 98 more rows

# Remove frosting from the workflow and predict
wf3 <- wf2 %>% remove_frosting()
p3 <- predict(wf3, latest)
p3
#> An `epi_df` object, 108 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 108 × 3
#>    geo_value time_value .pred
#>    <chr>     <date>     <dbl>
#>  1 ak        2021-12-10    NA
#>  2 ca        2021-12-10    NA
#>  3 ny        2021-12-10    NA
#>  4 ak        2021-12-11    NA
#>  5 ca        2021-12-11    NA
#>  6 ny        2021-12-11    NA
#>  7 ak        2021-12-12    NA
#>  8 ca        2021-12-12    NA
#>  9 ny        2021-12-12    NA
#> 10 ak        2021-12-13    NA
#> # ℹ 98 more rows
```
