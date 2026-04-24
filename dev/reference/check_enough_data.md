# Check the dataset contains enough data points.

`check_enough_data` creates a *specification* of a recipe operation that
will check if variables contain enough data.

## Usage

``` r
check_enough_data(
  recipe,
  ...,
  min_observations = NULL,
  epi_keys = NULL,
  drop_na = TRUE,
  role = NA,
  trained = FALSE,
  skip = TRUE,
  id = rand_id("enough_data")
)
```

## Arguments

- recipe:

  A recipe object. The check will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this check. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details. You will usually want to use
  [`recipes::all_predictors()`](https://recipes.tidymodels.org/reference/has_role.html)
  and/or
  [`recipes::all_outcomes()`](https://recipes.tidymodels.org/reference/has_role.html)
  here.

- min_observations:

  The minimum number of data points required for training. If this is
  NULL, the total number of predictors will be used.

- epi_keys:

  A character vector of column names on which to group the data and
  check threshold within each group. Useful if your forecaster trains
  per group (for example, per geo_value).

- drop_na:

  A logical for whether to count NA values as valid rows.

- role:

  Not used by this check since no new variables are created.

- trained:

  A logical for whether the selectors in `...` have been resolved by
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html).

- skip:

  A logical. If `TRUE`, only training data is checked, while if `FALSE`,
  both training and predicting data is checked. Technically, this
  answers the question "should the check be skipped when the recipe is
  baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)?" While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A character string that is unique to this check to identify it.

## Details

This check will break the `prep` and/or bake function if any of the
checked columns have not enough non-NA values. If the check passes,
nothing is changed in the data. It is best used after every other step.

For checking training data, it is best to set `...` to be
`all_predictors(), all_outcomes()`, while for checking prediction data,
it is best to set `...` to be `all_predictors()` only, with `n = 1`.

## tidy() results

When you
[`tidy()`](https://recipes.tidymodels.org/reference/tidy.recipe.html)
this check, a tibble with column `terms` (the selectors or variables
selected) is returned.
