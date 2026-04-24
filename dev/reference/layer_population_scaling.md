# Convert per-capita predictions to raw scale

`layer_population_scaling` creates a specification of a frosting layer
that will "undo" per-capita scaling done in
[`step_population_scaling()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_population_scaling.md).
Typical usage would set `df` to be a dataset that contains a list of
population for the `geo_value`s, and use it to convert predictions made
from a raw scale model to rate-scale by dividing by the population.
Although, it is worth noting that there is nothing special about
"population", and the function can be used to scale by any variable.
Population is the standard use case in the epidemiology forecasting
scenario. Any value passed will *multiply* the selected variables while
the `rate_rescaling` argument is a common *divisor* of the selected
variables.

## Usage

``` r
layer_population_scaling(
  frosting,
  ...,
  df,
  by = NULL,
  df_pop_col,
  rate_rescaling = 1,
  create_new = TRUE,
  suffix = "_scaled",
  id = rand_id("population_scaling")
)
```

## Arguments

- frosting:

  a `frosting` postprocessor. The layer will be added to the sequence of
  operations for this frosting.

- ...:

  One or more selector functions to scale variables for this step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- df:

  a data frame that contains the population data to be used for
  inverting the existing scaling.

- by:

  A (possibly named) character vector of variables to join `df` onto the
  `epi_df` by.

  If `NULL`, the default, the function will try to infer a reasonable
  set of columns. First, it will try to join by all variables in the
  test data with roles `"geo_value"`, `"key"`, or `"time_value"` that
  also appear in `df`; these roles are automatically set if you are
  using an `epi_df`, or you can use, e.g., `update_role`. If no such
  roles are set, it will try to perform a natural join, using variables
  in common between the training/test data and population data.

  If columns in the training/testing data and `df` have the same name
  (and aren't included in `by`), a `.df` suffix is added to the one from
  the user-provided data to disambiguate.

  To join by different variables on the `epi_df` and `df`, use a named
  vector. For example, `by = c("geo_value" = "states")` will match
  `epi_df$geo_value` to `df$states`. To join by multiple variables, use
  a vector with length \> 1. For example,
  `by = c("geo_value" = "states", "county" = "county")` will match
  `epi_df$geo_value` to `df$states` and `epi_df$county` to `df$county`.

  See
  [`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  for more details.

- df_pop_col:

  the name of the column in the data frame `df` that contains the
  population data and used for scaling.

- rate_rescaling:

  Sometimes rates are "per 100K" or "per 1M" rather than "per person".
  Adjustments can be made here. For example, if the original rate is
  "per 100K", then set `rate_rescaling = 1e5` to get counts back.

- create_new:

  TRUE to create a new column and keep the original column in the
  `epi_df`.

- suffix:

  a character. The suffix added to the column name if
  `create_new = TRUE`. Default to "\_scaled".

- id:

  a random id string

## Value

an updated `frosting` postprocessor

## Examples

``` r
jhu <- cases_deaths_subset %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
  select(geo_value, time_value, cases)

pop_data <- data.frame(states = c("ca", "ny"), value = c(20000, 30000))

r <- epi_recipe(jhu) %>%
  step_population_scaling(
    df = pop_data,
    df_pop_col = "value",
    by = c("geo_value" = "states"),
    cases, suffix = "_scaled"
  ) %>%
  step_epi_lag(cases_scaled, lag = c(0, 7, 14)) %>%
  step_epi_ahead(cases_scaled, ahead = 7, role = "outcome") %>%
  step_epi_naomit()

f <- frosting() %>%
  layer_predict() %>%
  layer_threshold(.pred) %>%
  layer_naomit(.pred) %>%
  layer_population_scaling(.pred,
    df = pop_data,
    by = c("geo_value" = "states"),
    df_pop_col = "value"
  )

wf <- epi_workflow(r, linear_reg()) %>%
  fit(jhu) %>%
  add_frosting(f)

forecast(wf)
#> An `epi_df` object, 2 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2024-03-20
#> Latency (lag between last available observation and epi_df's as_of, by time series):
#> * lag across all time series = 810 days
#> 
#> # A tibble: 2 × 4
#>   geo_value time_value .pred .pred_scaled
#>   <chr>     <date>     <dbl>        <dbl>
#> 1 ca        2021-12-31  4.25       84938.
#> 2 ny        2021-12-31  5.93      177766.
```
