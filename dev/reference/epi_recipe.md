# Create a epi_recipe for preprocessing data

A recipe is a description of the steps to be applied to a data set in
order to prepare it for data analysis. This is a loose wrapper around
[`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html)
to properly handle the additional columns present in an `epi_df`

## Usage

``` r
epi_recipe(x, ...)

# S3 method for class 'epi_df'
epi_recipe(
  x,
  reference_date = NULL,
  formula = NULL,
  ...,
  vars = NULL,
  roles = NULL
)

# S3 method for class 'formula'
epi_recipe(formula, data, reference_date = NULL, ...)
```

## Arguments

- x, data:

  An epi_df of the *template* data set (see below).

- ...:

  Further arguments passed to or from other methods (not currently
  used).

- reference_date:

  Either a date of the same class as the `time_value` column in the
  `epi_df` or `NULL`. If a date, it gives the date to which all
  operations are relative. Typically, in real-time tasks this is the
  date that the model is created (and presumably trained). In
  forecasting, this is often the same as the most recent date of data
  availability, but when data is "latent" (reported after the date to
  which it corresponds), or if performing a nowcast, the
  `reference_date` may be later than this. Setting `reference_date` to a
  value BEFORE the most recent data is not a true "forecast", because
  future data is being used to create the model, but this may be
  reasonable in model building, nowcasting (predicting finalized values
  from preliminary data), or if producing a backcast. If `NULL`, it will
  be set to the `as_of` date of the `epi_df`.

- formula:

  A model formula. No in-line functions should be used here (e.g.
  `log(x)`, `x:y`, etc.) and minus signs are not allowed. These types of
  transformations should be enacted using `step` functions in this
  package. Dots are allowed as are simple multivariate outcome terms
  (i.e. no need for `cbind`; see Examples).

- vars:

  A character string of column names corresponding to variables that
  will be used in any context (see below)

- roles:

  A character string (the same length of `vars`) that describes a single
  role that the variable will take. This value could be anything but
  common roles are `"outcome"`, `"predictor"`, `"time_value"`, and
  `"geo_value"`

## Value

An object of class `recipe` with sub-objects:

- var_info:

  A tibble containing information about the original data set columns.

- term_info:

  A tibble that contains the current set of terms in the data set. This
  initially defaults to the same data contained in `var_info`.

- steps:

  A list of `step` or `check` objects that define the sequence of
  preprocessing operations that will be applied to data. The default
  value is `NULL`.

- template:

  A tibble of the data. This is initialized to be the same as the data
  given in the `data` argument but can be different after the recipe is
  trained.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-08-01")

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
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
#> 1. Lagging: death_rate by 0, 7, 14
#> 2. Leading: death_rate by 7
#> 3. Lagging: case_rate by 0, 7, 14
#> 4. • Removing rows with NA values in: all_predictors()
#> 5. • Removing rows with NA values in: all_outcomes()
```
