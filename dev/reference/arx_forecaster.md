# Direct autoregressive forecaster with covariates

This is an autoregressive forecasting model for
[epiprocess::epi_df](https://cmu-delphi.github.io/epiprocess/reference/epi_df.html)
data. It does "direct" forecasting, meaning that it estimates a model
for a particular target horizon of the `outcome` based on the lags of
the `predictors`. See the [Get started
vignette](https://cmu-delphi.github.io/epipredict/dev/articles/epipredict.md)
for some worked examples and [Custom epi_workflows
vignette](https://cmu-delphi.github.io/epipredict/dev/articles/custom_epiworkflows.md)
for a recreation using a custom
[`epi_workflow()`](https://cmu-delphi.github.io/epipredict/dev/reference/epi_workflow.md).

## Usage

``` r
arx_forecaster(
  epi_data,
  outcome,
  predictors = outcome,
  trainer = linear_reg(),
  args_list = arx_args_list()
)
```

## Arguments

- epi_data:

  An `epi_df` object

- outcome:

  A character (scalar) specifying the outcome (in the `epi_df`).

- predictors:

  A character vector giving column(s) of predictor variables. This
  defaults to the `outcome`. However, if manually specified, only those
  variables specifically mentioned will be used, and the `outcome` will
  not be added.

- trainer:

  A `{parsnip}` model describing the type of estimation. For now, we
  enforce `mode = "regression"`.

- args_list:

  A list of customization arguments to determine the type of forecasting
  model. See
  [`arx_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_args_list.md).

## Value

An `arx_fcast`, with the fields `predictions` and `epi_workflow`.
`predictions` is a `tibble` of predicted values while
[`epi_workflow()`](https://cmu-delphi.github.io/epipredict/dev/reference/epi_workflow.md)
is the fit workflow used to make those predictions

## See also

[`arx_fcast_epi_workflow()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_fcast_epi_workflow.md),
[`arx_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_args_list.md)

## Examples

``` r
jhu <- covid_case_death_rates %>%
  dplyr::filter(time_value >= as.Date("2021-12-01"))

out <- arx_forecaster(
  jhu,
  "death_rate",
  c("case_rate", "death_rate")
)

out <- arx_forecaster(jhu,
  "death_rate",
  c("case_rate", "death_rate"),
  trainer = quantile_reg(),
  args_list = arx_args_list(quantile_levels = 1:9 / 10)
)
out
#> ══ A basic forecaster of type ARX Forecaster ═══════════════════════════════════
#> 
#> This forecaster was fit on 2026-06-30 23:39:34.
#> 
#> Training data was an <epi_df> with:
#> • Geography: state,
#> • Time type: day,
#> • Using data up-to-date as of: 2023-03-10.
#> • With the last data available on 2021-12-31
#> 
#> ── Predictions ─────────────────────────────────────────────────────────────────
#> 
#> A total of 56 predictions are available for
#> • 56 unique geographic regions,
#> • At forecast date: 2021-12-31,
#> • For target date: 2022-01-07,
#> 
```
