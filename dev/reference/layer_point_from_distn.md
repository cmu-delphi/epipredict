# Converts distributional forecasts to point forecasts

This function adds a post-processing layer to extract a point forecast
from a distributional forecast. NOTE: With default arguments, this will
remove information, so one should usually call this AFTER
[`layer_quantile_distn()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_quantile_distn.md)
or set the `name` argument to something specific.

## Usage

``` r
layer_point_from_distn(
  frosting,
  ...,
  type = c("median", "mean"),
  name = NULL,
  id = rand_id("point_from_distn")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor

- ...:

  Unused, include for consistency with other layers.

- type:

  character. Either `mean` or `median`.

- name:

  character. The name for the output column. The default `NULL` will
  overwrite the `.pred` column, removing the distribution information.

- id:

  a random id string

## Value

an updated `frosting` postprocessor.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, quantile_reg(quantile_levels = c(.25, .5, .75))) %>%
  fit(jhu)

f1 <- frosting() %>%
  layer_predict() %>%
  layer_quantile_distn() %>% # puts the other quantiles in a different col
  layer_point_from_distn() %>% # mutate `.pred` to contain only a point prediction
  layer_naomit(.pred)
wf1 <- wf %>% add_frosting(f1)

p1 <- forecast(wf1)
p1
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 3 × 4
#>   geo_value time_value  .pred .pred_distn
#>   <chr>     <date>      <dbl>   <qtls(7)>
#> 1 ak        2021-12-31 0.0580     [0.058]
#> 2 ca        2021-12-31 0.179      [0.179]
#> 3 ny        2021-12-31 0.274      [0.274]

f2 <- frosting() %>%
  layer_predict() %>%
  layer_point_from_distn() %>% # mutate `.pred` to contain only a point prediction
  layer_naomit(.pred)
wf2 <- wf %>% add_frosting(f2)

p2 <- forecast(wf2)
p2
#> An `epi_df` object, 3 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 3 × 3
#>   geo_value time_value  .pred
#>   <chr>     <date>      <dbl>
#> 1 ak        2021-12-31 0.0580
#> 2 ca        2021-12-31 0.179 
#> 3 ny        2021-12-31 0.274 
```
