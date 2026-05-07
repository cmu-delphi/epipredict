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
r
#> 
#> ── Epi Recipe ──────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations 
#> 1. Calculating lag_difference for: case_rate death_rate by 7, 14
#> 2. • Removing rows with NA values in: all_predictors()
#> 3. • Removing rows with NA values in: all_outcomes()

r %>%
  prep(covid_case_death_rates) %>%
  bake(new_data = NULL)
#> An `epi_df` object, 19,712 x 8 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 434 days
#> 
#> # A tibble: 19,712 × 8
#>    geo_value time_value case_rate death_rate lag_diff_7_case_rate
#>  * <chr>     <date>         <dbl>      <dbl>                <dbl>
#>  1 ak        2021-01-14      37.4     0.0988               -4.07 
#>  2 al        2021-01-14      73.6     2.51                 -7.70 
#>  3 ar        2021-01-14      87.7     1.42                -10.3  
#>  4 as        2021-01-14       0       0                     0    
#>  5 az        2021-01-14     124.      2.14                  0.117
#>  6 ca        2021-01-14     108.      1.22                  2.38 
#>  7 co        2021-01-14      31.7     0.354                -9.62 
#>  8 ct        2021-01-14      75.4     1.07                  6.97 
#>  9 dc        2021-01-14      41.5     0.681                 6.07 
#> 10 de        2021-01-14      74.1     0.955                -5.46 
#> # ℹ 19,702 more rows
#> # ℹ 3 more variables: lag_diff_14_case_rate <dbl>, lag_diff_7_death_rate <dbl>,
#> #   lag_diff_14_death_rate <dbl>
```
