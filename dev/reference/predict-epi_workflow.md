# Predict from an epi_workflow

This is the [`predict()`](https://rdrr.io/r/stats/predict.html) method
for a fit epi_workflow object. The 3 steps that this implements are:

- Preprocess `new_data` using the preprocessing method specified when
  the workflow was created and fit. This is accomplished using
  [`hardhat::forge()`](https://hardhat.tidymodels.org/reference/forge.html),
  which will apply any formula preprocessing or call
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)
  if a recipe was supplied.

- Preprocessing `new_data` using the preprocessing method specified when
  the epi_workflow was created and fit. This is accomplished using
  `hardhat::bake()` if a recipe was supplied (passing through
  [`hardhat::forge()`](https://hardhat.tidymodels.org/reference/forge.html),
  which is used for non-recipe preprocessors). Note that this is a
  slightly different `bake` operation than the one occuring during the
  fit. Any `step` that has `skip = TRUE` isn't applied during
  prediction; for example in
  [`step_epi_naomit()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_naomit.md),
  [`all_outcomes()`](https://recipes.tidymodels.org/reference/has_role.html)
  isn't `NA` omitted, since doing so would drop the exact `time_values`
  we are trying to predict.

- Calling
  [`parsnip::predict.model_fit()`](https://parsnip.tidymodels.org/reference/predict.model_fit.html)
  for you using the underlying fit parsnip model.

- [`slather()`](https://cmu-delphi.github.io/epipredict/dev/reference/slather.md)
  any frosting that has been included in the `epi_workflow`.

## Usage

``` r
# S3 method for class 'epi_workflow'
predict(object, new_data, type = NULL, opts = list(), ...)
```

## Arguments

- object:

  An epi_workflow that has been fit by
  [`workflows::fit.workflow()`](https://workflows.tidymodels.org/reference/fit-workflow.html)

- new_data:

  A data frame containing the new predictors to preprocess and predict
  on

- type:

  A single character value or `NULL`. Possible values are `"numeric"`,
  `"class"`, `"prob"`, `"conf_int"`, `"pred_int"`, `"quantile"`,
  `"time"`, `"hazard"`, `"survival"`, or `"raw"`. When `NULL`,
  [`predict()`](https://rdrr.io/r/stats/predict.html) will choose an
  appropriate value based on the model's mode.

- opts:

  A list of optional arguments to the underlying predict function that
  will be used when `type = "raw"`. The list should not include options
  for the model object or the new data being predicted.

- ...:

  Additional `parsnip`-related options, depending on the value of
  `type`. Arguments to the underlying model's prediction function cannot
  be passed here (use the `opts` argument instead). Possible arguments
  are:

  - `interval`: for `type` equal to `"survival"` or `"quantile"`, should
    interval estimates be added, if available? Options are `"none"` and
    `"confidence"`.

  - `level`: for `type` equal to `"conf_int"`, `"pred_int"`, or
    `"survival"`, this is the parameter for the tail area of the
    intervals (e.g. confidence level for confidence intervals). Default
    value is `0.95`.

  - `std_error`: for `type` equal to `"conf_int"` or `"pred_int"`, add
    the standard error of fit or prediction (on the scale of the linear
    predictors). Default value is `FALSE`.

  - `quantile`: for `type` equal to `quantile`, the quantiles of the
    distribution. Default is `(1:9)/10`.

  - `eval_time`: for `type` equal to `"survival"` or `"hazard"`, the
    time points at which the survival probability or hazard is
    estimated.

## Value

A data frame of model predictions, with as many rows as `new_data` has.
If `new_data` is an
[`epiprocess::epi_df`](https://cmu-delphi.github.io/epiprocess/reference/epi_df.html)
or a data frame with `time_value` or `geo_value` columns, then the
result will have those as well.

## Examples

``` r
jhu <- covid_case_death_rates

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_epi_naomit()

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- jhu %>% dplyr::filter(time_value >= max(time_value) - 14)

preds <- predict(wf, latest)
preds
#> An `epi_df` object, 56 x 3 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> Latency (time between last available observation and epi_df's as_of, by time series):
#> * latency  = 434 days
#> 
#> # A tibble: 56 × 3
#>    geo_value time_value   .pred
#>    <chr>     <date>       <dbl>
#>  1 ak        2021-12-31 0.362  
#>  2 al        2021-12-31 0.255  
#>  3 ar        2021-12-31 0.431  
#>  4 as        2021-12-31 0.00105
#>  5 az        2021-12-31 0.647  
#>  6 ca        2021-12-31 0.276  
#>  7 co        2021-12-31 0.718  
#>  8 ct        2021-12-31 0.614  
#>  9 dc        2021-12-31 1.00   
#> 10 de        2021-12-31 0.709  
#> # ℹ 46 more rows
```
