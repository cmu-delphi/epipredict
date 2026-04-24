# Recipe blueprint that accounts for `epi_df` panel data

Used for simplicity. See
[`hardhat::new_recipe_blueprint()`](https://hardhat.tidymodels.org/reference/new-blueprint.html)
or
[`hardhat::default_recipe_blueprint()`](https://hardhat.tidymodels.org/reference/default_recipe_blueprint.html)
for more details.

## Usage

``` r
new_epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble",
  ptypes = NULL,
  recipe = NULL,
  ...,
  subclass = character()
)

epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble"
)

default_epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble"
)

new_default_epi_recipe_blueprint(
  intercept = FALSE,
  allow_novel_levels = FALSE,
  fresh = TRUE,
  composition = "tibble",
  ptypes = NULL,
  recipe = NULL,
  extra_role_ptypes = NULL,
  ...,
  subclass = character()
)
```

## Arguments

- intercept:

  A logical. Should an intercept be included in the processed data? This
  information is used by the `process` function in the `mold` and
  `forge` function list.

- allow_novel_levels:

  A logical. Should novel factor levels be allowed at prediction time?
  This information is used by the `clean` function in the `forge`
  function list, and is passed on to
  [`scream()`](https://hardhat.tidymodels.org/reference/scream.html).

- fresh:

  Should already trained operations be re-trained when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is
  called?

- composition:

  Either "tibble", "matrix", or "dgCMatrix" for the format of the
  processed predictors. If "matrix" or "dgCMatrix" are chosen, all of
  the predictors must be numeric after the preprocessing method has been
  applied; otherwise an error is thrown.

- ptypes:

  Either `NULL`, or a named list with 2 elements, `predictors` and
  `outcomes`, both of which are 0-row tibbles. `ptypes` is generated
  automatically at
  [`mold()`](https://hardhat.tidymodels.org/reference/mold.html) time
  and is used to validate `new_data` at prediction time.

- recipe:

  Either `NULL`, or an unprepped recipe. This argument is set
  automatically at
  [`mold()`](https://hardhat.tidymodels.org/reference/mold.html) time.

- ...:

  Name-value pairs for additional elements of blueprints that subclass
  this blueprint.

- subclass:

  A character vector. The subclasses of this blueprint.

- extra_role_ptypes:

  A named list. The names are the unique non-standard recipe roles (i.e.
  everything except `"predictors"` and `"outcomes"`). The values are
  prototypes of the original columns with that role. These are used for
  validation in `forge()`.

## Value

A recipe blueprint.

## Details

The `bake_dependent_roles` are automatically set to `epi_df` defaults.
