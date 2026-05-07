# Returns predictive quantiles

This function calculates quantiles when the prediction was
*distributional*. If the model producing the forecast is not
distributional, it is recommended to use
[`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_residual_quantiles.md)
instead.

## Usage

``` r
layer_quantile_distn(
  frosting,
  ...,
  quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  truncate = c(-Inf, Inf),
  name = ".pred_distn",
  id = rand_id("quantile_distn")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor

- ...:

  Unused, include for consistency with other layers.

- quantile_levels:

  a vector of probabilities to extract

- truncate:

  Do we truncate the distribution to an interval

- name:

  character. The name for the output column.

- id:

  a random id string

## Value

an updated `frosting` postprocessor. An additional column of predictive
quantiles will be added to the predictions.

## Details

Currently, the only distributional modes/engines are

- [`quantile_reg()`](https://cmu-delphi.github.io/epipredict/dev/reference/quantile_reg.md)

- [`smooth_quantile_reg()`](https://cmu-delphi.github.io/epipredict/dev/reference/smooth_quantile_reg.md)

- `rand_forest(mode = "regression") %>% set_engine("grf_quantiles")`

If these engines were used, then this layer will grab out estimated (or
extrapolated) quantiles at the requested quantile values.

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

f <- frosting() %>%
  layer_predict() %>%
  layer_quantile_distn() %>%
  layer_naomit(.pred)
wf1 <- wf %>% add_frosting(f)

p <- forecast(wf1)
p
#> An `epi_df` object, 3 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * No time series detected
#> # A tibble: 3 × 4
#>   geo_value time_value     .pred .pred_distn
#>   <chr>     <date>     <qtls(3)>   <qtls(7)>
#> 1 ak        2021-12-31   [0.058]     [0.058]
#> 2 ca        2021-12-31   [0.179]     [0.179]
#> 3 ny        2021-12-31   [0.274]     [0.274]
```
