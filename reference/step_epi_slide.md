# Calculate a rolling window transformation

`step_epi_slide()` creates a *specification* of a recipe step that will
generate one or more new columns of derived data by "sliding" a
computation along existing data. This is a wrapper around
[`epiprocess::epi_slide()`](https://cmu-delphi.github.io/epiprocess/reference/epi_slide.html)
to allow its use within an
[`epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/epi_recipe.md).

## Usage

``` r
step_epi_slide(
  recipe,
  ...,
  .f,
  .window_size = NULL,
  .align = c("right", "center", "left"),
  role = "predictor",
  prefix = "epi_slide_",
  f_name = clean_f_name(.f),
  skip = FALSE,
  id = rand_id("epi_slide")
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

- .f:

  A function in one of the following formats:

  1.  An unquoted function name with no arguments, e.g., `mean`

  2.  A character string name of a function, e.g., `"mean"`. Note that
      this can be difficult to examine for mistakes (so the misspelling
      `"maen"` won't produce an error until you try to actually fit the
      model)

  3.  A base `R` lambda function, e.g.,
      `function(x) mean(x, na.rm = TRUE)`

  4.  A new-style base `R` lambda function, e.g.,
      `\(x) mean(x, na.rm = TRUE)`

  5.  A one-sided formula like `~ mean(.x, na.rm = TRUE)`.

  Note that in cases 3 and 4, `x` can be any variable name you like (for
  example `\(dog) mean(dog, na.rm = TRUE)` will work). But in case 5,
  the argument must be named `.x`. A common, though very difficult to
  debug error is using something like `function(x) mean`. This will not
  work because it returns the function mean, rather than `mean(x)`

- .window_size:

  the size of the sliding window, required. Usually a non-negative
  integer will suffice (e.g. for data indexed by date, but more
  restrictive in other time_type cases (see
  [`epiprocess::epi_slide()`](https://cmu-delphi.github.io/epiprocess/reference/epi_slide.html)
  for details). For example, set to 7 for a 7-day window.

- .align:

  a character string indicating how the window should be aligned. By
  default, this is "right", meaning the slide_window will be anchored
  with its right end point on the reference date. (see
  [`epiprocess::epi_slide()`](https://cmu-delphi.github.io/epiprocess/reference/epi_slide.html)
  for details).

- role:

  For model terms created by this step, what analysis role should they
  be assigned? `lag` is default a predictor while `ahead` is an outcome.

- prefix:

  A character string that will be prefixed to the new column.

- f_name:

  a character string of at most 20 characters that describes the
  function. This will be combined with `prefix` and the columns in `...`
  to name the result using `{prefix}{f_name}_{column}`. By default it
  will be determined automatically using
  [`clean_f_name()`](https://cmu-delphi.github.io/epipredict/reference/clean_f_name.md).

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

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-01-01"), geo_value %in% c("ca", "ny"))
rec <- epi_recipe(jhu) %>%
  step_epi_slide(case_rate, death_rate,
    .f = \(x) mean(x, na.rm = TRUE),
    .window_size = 7L
  )
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
bake(prep(rec, jhu), new_data = NULL)
#> Error: object 'rec' not found
```
