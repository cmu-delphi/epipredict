# Create frosting for post-processing predictions

This generates a post-processing container (much like
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html))
to hold steps for post-processing predictions.

## Usage

``` r
frosting(layers = NULL, requirements = NULL)
```

## Arguments

- layers:

  Must be `NULL`.

- requirements:

  Must be `NULL`.

## Value

A frosting object.

## Details

The arguments are currently placeholders and must be NULL

## Examples

``` r
# Toy example to show that frosting can be created and added for post-processing
f <- frosting()
wf <- epi_workflow() %>% add_frosting(f)

# A more realistic example
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)

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
