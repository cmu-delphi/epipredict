# Limits the size of the training window to the most recent observations

`step_training_window` creates a *specification* of a recipe step that
limits the size of the training window to the `n_recent` most recent
observations in `time_value` per group, where the groups are formed
based on the remaining `epi_keys`.

## Usage

``` r
step_training_window(
  recipe,
  role = NA,
  n_recent = 50,
  epi_keys = NULL,
  id = rand_id("training_window")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- role:

  For model terms created by this step, what analysis role should they
  be assigned? `lag` is default a predictor while `ahead` is an outcome.

- n_recent:

  An integer value that represents the number of most recent
  observations that are to be kept in the training window per key The
  default value is 50.

- epi_keys:

  An optional character vector for specifying "key" variables to group
  on. The default, `NULL`, ensures that every key combination is
  limited.

- id:

  A unique identifier for the step

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

It is recommended to do this after any
[`step_epi_ahead()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md),
[`step_epi_lag()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md),
or
[`step_epi_naomit()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_naomit.md)
steps. If `step_training_window()` happens first, there will be less
than `n_training` remaining examples, since either leading or lagging
will introduce `NA`'s later removed by
[`step_epi_naomit()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_naomit.md).
Typical usage will use this step last in an
[`epi_recipe()`](https://cmu-delphi.github.io/epipredict/dev/reference/epi_recipe.md).

## Examples

``` r
tib <- tibble(
  x = 1:10,
  y = 1:10,
  time_value = rep(seq(as.Date("2020-01-01"), by = 1, length.out = 5), 2),
  geo_value = rep(c("ca", "hi"), each = 5)
) %>%
  as_epi_df()

epi_recipe(y ~ x, data = tib) %>%
  step_training_window(n_recent = 3) %>%
  prep(tib) %>%
  bake(new_data = NULL)
#> An `epi_df` object, 6 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2026-05-09 20:53:11.144364
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 2316 days
#> 
#> # A tibble: 6 × 4
#>   geo_value time_value     x     y
#> * <chr>     <date>     <int> <int>
#> 1 ca        2020-01-03     3     3
#> 2 ca        2020-01-04     4     4
#> 3 ca        2020-01-05     5     5
#> 4 hi        2020-01-03     8     8
#> 5 hi        2020-01-04     9     9
#> 6 hi        2020-01-05    10    10

epi_recipe(y ~ x, data = tib) %>%
  step_epi_naomit() %>%
  step_training_window(n_recent = 3) %>%
  prep(tib) %>%
  bake(new_data = NULL)
#> An `epi_df` object, 6 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2026-05-09 20:53:11.144364
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency across all time series = 2316 days
#> 
#> # A tibble: 6 × 4
#>   geo_value time_value     x     y
#> * <chr>     <date>     <int> <int>
#> 1 ca        2020-01-03     3     3
#> 2 ca        2020-01-04     4     4
#> 3 ca        2020-01-05     5     5
#> 4 hi        2020-01-03     8     8
#> 5 hi        2020-01-04     9     9
#> 6 hi        2020-01-05    10    10
```
