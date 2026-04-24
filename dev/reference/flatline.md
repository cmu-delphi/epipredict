# (Internal) implementation of the flatline forecaster

This is an internal function that is used to create a
[`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
model. It has somewhat odd behaviour (see below).

## Usage

``` r
flatline(formula, data)
```

## Arguments

- formula:

  The lhs should be a single variable. In standard usage, this would
  actually be the observed time series shifted forward by the forecast
  horizon. The right hand side must contain any keys (locations) for the
  panel data separated by plus. The observed time series must come last.
  For example

      form <- as.formula(lead7_y ~ state + age + y)

  Note that this function doesn't DO the shifting, that has to be done
  outside.

- data:

  A data frame containing at least the variables used in the formula. It
  must also contain a column `time_value` giving the observed time
  points.

## Value

An S3 object of class `flatline` with two components:

- `residuals` - a tibble with all keys and a `.resid` column that
  contains forecast errors.

- `.pred` - a tibble with all keys and a `.pred` column containing only
  predictions for future data (the last observed of the outcome for each
  combination of keys.

## Examples

``` r
tib <- data.frame(
  y = runif(100),
  expand.grid(k = letters[1:4], j = letters[5:9], time_value = 1:5)
) %>%
  dplyr::group_by(k, j) %>%
  dplyr::mutate(y2 = dplyr::lead(y, 2)) # predict 2 steps ahead
flat <- flatline(y2 ~ j + k + y, tib) # predictions for 20 locations
sum(!is.na(flat$residuals$.resid)) # 100 residuals, but 40 are NA
#> [1] 60
```
