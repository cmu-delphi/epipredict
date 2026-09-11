# Spread a layer of frosting on a fitted workflow

Slathering frosting means to implement a post-processing layer. It is
the post-processing equivalent of `bake` for a recipe. Given a layer, it
applies the actual transformation of that layer. When creating a new
post-processing layer, you must implement an S3 method for this
function. Generally, you will not need to call this function directly,
as it will be used indirectly during `predict`.

## Usage

``` r
slather(object, components, workflow, new_data, ...)
```

## Arguments

- object:

  a workflow with `frosting` post-processing steps

- components:

  a list of components containing model information. These will be
  updated and returned by the layer. These should be

  - `mold` - the output of calling
    [`hardhat::mold()`](https://hardhat.tidymodels.org/reference/mold.html)
    on the workflow. This contains information about the preprocessing,
    including the recipe.

  - `forged` - the output of calling
    [`hardhat::forge()`](https://hardhat.tidymodels.org/reference/forge.html)
    on the workflow. This should have predictors and outcomes for the
    `new_data`. It will have three components `predictors`, `outcomes`
    (if these were in the `new_data`), and `extras` (usually has the
    rest of the data, including `keys`).

  - `keys` - we put the keys (`time_value`, `geo_value`, and any others)
    here for ease.

- workflow:

  an object of class workflow

- new_data:

  a data frame containing the new predictors to preprocess and predict
  on

- ...:

  additional arguments used by methods. Currently unused.

## Value

The `components` list, in the same format as before, after applying any
updates.
