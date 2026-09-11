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
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
#> Error: object 'r' not found
latest <- jhu %>% filter(time_value >= max(time_value) - 14)

# Specify a `forecast_date` that is greater than or equal to `as_of` date
f <- frosting() %>%
  layer_predict() %>%
  layer_add_forecast_date(forecast_date = "2022-05-31") %>%
  layer_naomit(.pred)

wf1 <- wf %>% add_frosting(f)
#> Error: object 'wf' not found

p1 <- predict(wf1, latest)
#> Error: object 'wf1' not found
p1
#> Error: object 'p1' not found

# Update forecast date
f$layers[[2]] <- update(f$layers[[2]], forecast_date = "2021-06-01")

# Need to still update workflow if only update a layer in frosting
wf2 <- wf %>% add_frosting(f)
#> Error: object 'wf' not found
wf2$post # Check that wf1 has update
#> Error: object 'wf2' not found
p1 <- predict(wf2, latest)
#> Error: object 'wf2' not found
p1
#> Error: object 'p1' not found
```
