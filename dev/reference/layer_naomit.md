# Omit `NA`s from predictions or other columns

Omit `NA`s from predictions or other columns

## Usage

``` r
layer_naomit(frosting, ..., id = rand_id("naomit"))
```

## Arguments

- frosting:

  a `frosting` postprocessor

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.
  Typical usage is `.pred` to remove any rows with `NA` predictions.

- id:

  a random id string

## Value

an updated `frosting` postprocessor

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7)

wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)

f <- frosting() %>%
  layer_predict() %>%
  layer_naomit(.pred)

wf1 <- wf %>% add_frosting(f)

p <- forecast(wf1)
p
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
```
