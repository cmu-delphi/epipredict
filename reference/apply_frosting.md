# Apply post-processing to a fitted workflow

This function is intended for internal use. It implements
post-processing inside of the
[`predict()`](https://rdrr.io/r/stats/predict.html) method for a fitted
workflow.

## Usage

``` r
apply_frosting(workflow, ...)

# Default S3 method
apply_frosting(workflow, components, ...)

# S3 method for class 'epi_workflow'
apply_frosting(workflow, components, new_data, type = NULL, opts = list(), ...)
```

## Arguments

- workflow:

  An object of class workflow

- ...:

  additional arguments passed on to methods

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

- new_data:

  a data frame containing the new predictors to preprocess and predict
  on

- type, opts:

  forwarded (along with `...`) to
  [`parsnip::predict.model_fit()`](https://parsnip.tidymodels.org/reference/predict.model_fit.html)
  and
  [`slather()`](https://cmu-delphi.github.io/epipredict/reference/slather.md)
  for supported layers
