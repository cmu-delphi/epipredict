# Convert raw scale predictions to per-capita

`step_population_scaling()` creates a specification of a recipe step
that will perform per-capita scaling. Typical usage would set `df` to be
a dataset that contains population for each `geo_value`, and use it to
convert predictions made from a raw scale model to rate-scale by
dividing by the population. Although, it is worth noting that there is
nothing special about "population", and the function can be used to
scale by any variable. Population is the standard use case in the
epidemiology forecasting scenario. Any value passed will *divide* the
selected variables while the `rate_rescaling` argument is a common
*multiplier* of the selected variables.

## Usage

``` r
step_population_scaling(
  recipe,
  ...,
  role = "raw",
  df,
  by = NULL,
  df_pop_col,
  rate_rescaling = 1,
  create_new = TRUE,
  suffix = "_scaled",
  skip = FALSE,
  id = rand_id("population_scaling")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- role:

  For model terms created by this step, what analysis role should they
  be assigned?

- df:

  a data frame containing the scaling data (typically population). The
  target column is divided by the value in `df_pop_col`.

- by:

  A (possibly named) character vector of variables by which to join `df`
  to the `epi_df`.

  If `NULL`, the default, the function will try to infer a reasonable
  set of columns. First, it will try to join by all variables in the
  training/test data with roles `"geo_value"`, `"key"`, or
  `"time_value"` that also appear in `df`; these roles are automatically
  set if you are using an `epi_df`, or you can use, e.g., `update_role`.
  If no such roles are set, it will try to perform a natural join, using
  variables in common between the training/test data and population
  data.

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
  [`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
  for more details.

- df_pop_col:

  the name of the column in the data frame `df` that contains the
  population data and will be used for scaling. This should be one
  column.

- rate_rescaling:

  Sometimes raw scales are "per 100K" or "per 1M". Adjustments can be
  made here. For example, if the original scale is "per 100K", then set
  `rate_rescaling = 1e5` to get rates.

- create_new:

  `TRUE` to create a new column and keep the original column in the
  `epi_df`

- suffix:

  a character. The suffix added to the column name if
  `create_new = TRUE`. Default to "\_scaled".

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A unique identifier for the step

## Value

Scales raw data by the population

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
