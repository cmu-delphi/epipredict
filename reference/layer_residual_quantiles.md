# Creates predictions based on residual quantiles

This function calculates predictive quantiles based on the empirical
quantiles of the model's residuals. If the model producing the forecast
is distributional, it is recommended to use `layer_residual_quantiles()`
instead, as those will be more accurate.

## Usage

``` r
layer_residual_quantiles(
  frosting,
  ...,
  quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  symmetrize = TRUE,
  by_key = character(0L),
  name = ".pred_distn",
  id = rand_id("residual_quantiles")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor

- ...:

  Unused, include for consistency with other layers.

- quantile_levels:

  numeric vector of probabilities with values in (0,1) referring to the
  desired quantile. Note that 0.5 will always be included even if left
  out by the user.

- symmetrize:

  logical. If `TRUE` then the interval will be symmetric. Typically, one
  would only want non-symmetric quantiles when increasing trajectories
  are quite different from decreasing ones, such as a strictly postive
  variable near zero.

- by_key:

  A character vector of keys to group the residuals by before
  calculating quantiles. The default,
  [`c()`](https://rdrr.io/r/base/c.html) performs no grouping.

- name:

  character. The name for the output column.

- id:

  a random id string

## Value

an updated `frosting` postprocessor with additional columns of the
residual quantiles added to the prediction

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)

f <- frosting() %>%
  layer_predict() %>%
  layer_residual_quantiles(
    quantile_levels = c(0.025, 0.975),
    symmetrize = FALSE
  ) %>%
  layer_naomit(.pred)
wf1 <- wf %>% add_frosting(f)

p <- forecast(wf1)

f2 <- frosting() %>%
  layer_predict() %>%
  layer_residual_quantiles(
    quantile_levels = c(0.3, 0.7),
    by_key = "geo_value"
  ) %>%
  layer_naomit(.pred)
wf2 <- wf %>% add_frosting(f2)

p2 <- forecast(wf2)
#> Warning: Some grouping keys are not in data.frame returned by the `residuals()` method.
#> Groupings may not be correct.
```
