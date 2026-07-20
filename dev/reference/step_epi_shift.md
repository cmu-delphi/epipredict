# Create a shifted predictor

`step_epi_lag` and `step_epi_ahead` create a *specification* of a recipe
step that will add new columns of shifted data. The `step_epi_lag` will
create a lagged `predictor` column, while `step_epi_ahead` will create a
leading `outcome` column. Shifted data will by default include NA values
where the shift was induced. These can be properly removed with
[`step_epi_naomit()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_naomit.md),
or you may specify an alternative value with the `default` argument.

## Usage

``` r
step_epi_lag(
  recipe,
  ...,
  lag,
  role = "predictor",
  prefix = "lag_",
  default = NA,
  skip = FALSE,
  id = rand_id("epi_lag")
)

step_epi_ahead(
  recipe,
  ...,
  ahead,
  role = "outcome",
  prefix = "ahead_",
  default = NA,
  skip = FALSE,
  id = rand_id("epi_ahead")
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

- lag, ahead:

  A vector of integers. Each specified column will be the lag or lead
  for each value in the vector. Lag integers must be nonnegative, while
  ahead integers must be positive.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? `lag` is default a predictor while `ahead` is an outcome.

- prefix:

  A character string that will be prefixed to the new column.

- default:

  Determines what fills empty rows left by leading/lagging (defaults to
  NA).

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

Our `lag/ahead` functions respect the `geo_value` and `other_keys` of
the `epi_df`, and allow for discontiguous `time_value`s. Both of these
features are noticably lacking from `recipe::step_lag()`. Our
`lag/ahead` functions also appropriately adjust the amount of data to
avoid accidentally dropping recent predictors from the test data.

The `prefix` and `id` arguments are unchangeable to ensure that the code
runs properly and to avoid inconsistency with naming. For
`step_epi_ahead`, they are always set to `"ahead_"` and `"epi_ahead"`
respectively, while for `step_epi_lag`, they are set to `"lag_"` and
`"epi_lag`, respectively.

## See also

Other row operation steps:
[`step_adjust_latency()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_adjust_latency.md),
[`step_growth_rate()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_growth_rate.md),
[`step_lag_difference()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_lag_difference.md)

Other row operation steps:
[`step_adjust_latency()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_adjust_latency.md),
[`step_growth_rate()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_growth_rate.md),
[`step_lag_difference()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_lag_difference.md)

## Examples

``` r
r <- epi_recipe(covid_case_death_rates) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14))
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
r
#> Error: object 'r' not found
```
