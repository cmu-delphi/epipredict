# Unnest prediction list-cols

For any model that produces forecasts for multiple outcomes, such as
multiple aheads, the resulting prediction is a list of forecasts inside
a column of the prediction tibble, which is may not be desirable. This
layer "lengthens" the result, moving each outcome to a separate row, in
the same manner as
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html)
would. At the moment, the only such engine is
[`smooth_quantile_reg()`](https://cmu-delphi.github.io/epipredict/dev/reference/smooth_quantile_reg.md).

## Usage

``` r
layer_unnest(frosting, ..., id = rand_id("unnest"))
```

## Arguments

- frosting:

  a `frosting` postprocessor

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Variable names
  can be used as if they were positions in the data frame, so
  expressions like `x:y` can be used to select a range of variables.

- id:

  a random id string

## Value

an updated `frosting` postprocessor

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

aheads <- 1:7

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = aheads) %>%
  step_epi_naomit()

wf <- epi_workflow(
  r,
  smooth_quantile_reg(
    quantile_levels = c(.05, .1, .25, .5, .75, .9, .95),
    outcome_locations = aheads
  )
) %>%
  fit(jhu)

f <- frosting() %>%
  layer_predict() %>%
  layer_naomit() %>%
  layer_unnest(.pred)

wf1 <- wf %>% add_frosting(f)

p <- forecast(wf1)
p
#> An `epi_df` object, 0 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> # A tibble: 0 × 3
#> # ℹ 3 variables: geo_value <chr>, time_value <date>, .pred <???>
```
