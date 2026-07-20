# Calculate a lagged difference

`step_lag_difference()` creates a *specification* of a recipe step that
will generate one or more new columns of derived data. For each column
in the specification, `step_lag_difference()` will calculate the
difference between the values at a distance of `horizon`. For example,
with `horizon=1`, this would simply be the difference between adjacent
days.

## Usage

``` r
step_lag_difference(
  recipe,
  ...,
  role = "predictor",
  horizon = 7,
  prefix = "lag_diff_",
  skip = FALSE,
  id = rand_id("lag_diff")
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
  be assigned? `lag` is default a predictor while `ahead` is an outcome.

- horizon:

  Scalar or vector. Time period(s) over which to calculate differences.

- prefix:

  A character string that will be prefixed to the new column.

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

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

Much like
[`step_epi_lag()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md)
this step works with the actual time values (so if there are gaps it
will fill with `NA` values), and respects the grouping inherent in the
`epi_df()` as specified by `geo_value` and `other_keys`.

## See also

Other row operation steps:
[`step_adjust_latency()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_adjust_latency.md),
[`step_epi_lag()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md),
[`step_growth_rate()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_growth_rate.md)

## Examples

``` r
r <- epi_recipe(covid_case_death_rates) %>%
  step_lag_difference(case_rate, death_rate, horizon = c(7, 14)) %>%
  step_epi_naomit()
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
r
#> Error: object 'r' not found

r %>%
  prep(covid_case_death_rates) %>%
  bake(new_data = NULL)
#> Error: object 'r' not found
```
