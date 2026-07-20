# Calculate a growth rate

`step_growth_rate()` creates a *specification* of a recipe step that
will generate one or more new columns of derived data. This is a wrapper
around
[`epiprocess::growth_rate()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate.html)
to allow its use within an
[`epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/epi_recipe.md).

## Usage

``` r
step_growth_rate(
  recipe,
  ...,
  role = "predictor",
  horizon = 7,
  method = c("rel_change", "linear_reg"),
  log_scale = FALSE,
  na_rm = TRUE,
  replace_Inf = NA,
  prefix = "gr_",
  skip = FALSE,
  id = rand_id("growth_rate")
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

  Bandwidth for the sliding window, when `method` is "rel_change" or
  "linear_reg". See
  [`epiprocess::growth_rate()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate.html)
  for more details.

- method:

  Either "rel_change" or "linear_reg", indicating the method to use for
  the growth rate calculation. These are local methods: they are run in
  a sliding fashion over the sequence (in order to estimate derivatives
  and hence growth rates). See
  [`epiprocess::growth_rate()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate.html)
  for more details.

- log_scale:

  Should growth rates be estimated using the parameterization on the log
  scale? See details for an explanation. Default is `FALSE`.

- na_rm:

  Should missing values be removed before the computation? Default is
  `FALSE`.

- replace_Inf:

  Sometimes, the growth rate calculation can result in infinite values
  (if the denominator is zero, for example). In this case, most
  prediction methods will fail. This argument specifies potential
  replacement values. The default (`NA`) will likely result in these
  rows being removed from the data. Alternatively, you could specify
  arbitrary large values, or perhaps zero. Setting this argument to
  `NULL` will result in no replacement.

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

## See also

Other row operation steps:
[`step_adjust_latency()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_adjust_latency.md),
[`step_epi_lag()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md),
[`step_lag_difference()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_lag_difference.md)

## Examples

``` r
tiny_geos <- c("as", "mp", "vi", "gu", "pr")
rates <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-11-01"), !(geo_value %in% tiny_geos))

r <- epi_recipe(rates) %>%
  step_growth_rate(case_rate, death_rate)
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
r
#> Error: object 'r' not found

r %>%
  prep(rates) %>%
  bake(new_data = NULL)
#> Error: object 'r' not found
```
