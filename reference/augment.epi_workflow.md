# Augment data with predictions

[`augment()`](https://generics.r-lib.org/reference/augment.html), unlike
[`forecast()`](https://generics.r-lib.org/reference/forecast.html), has
the goal of modifying the training data, rather than just producing new
forecasts. It does a prediction on `new_data`, which will produce a
prediction for most `time_values`, and then adds `.pred` as a column to
`new_data` and returns the resulting join.

## Usage

``` r
# S3 method for class 'epi_workflow'
augment(x, new_data, ...)
```

## Arguments

- x:

  A trained epi_workflow

- new_data:

  A epi_df of predictors

- ...:

  Arguments passed on to the predict method.

## Value

new_data with additional columns containing the predicted values
