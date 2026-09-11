# Create a template `arx_forecaster` workflow

This function creates an unfit workflow for use with
[`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md).
It is useful if you want to make small modifications to that forecaster
before fitting and predicting. Supplying a trainer to the function may
alter the returned `epi_workflow` object (e.g., if you intend to use
[`quantile_reg()`](https://cmu-delphi.github.io/epipredict/reference/quantile_reg.md))
but can be omitted.

## Usage

``` r
arx_fcast_epi_workflow(
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
  enforce `mode = "regression"`. May be `NULL` if you'd like to decide
  later.

- args_list:

  A list of customization arguments to determine the type of forecasting
  model. See
  [`arx_args_list()`](https://cmu-delphi.github.io/epipredict/reference/arx_args_list.md).

## Value

An unfitted `epi_workflow`.

## See also

[`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md),
[`arx_args_list()`](https://cmu-delphi.github.io/epipredict/reference/arx_args_list.md)

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-12-01"))

arx_fcast_epi_workflow(
  jhu, "death_rate",
  c("case_rate", "death_rate")
)
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: Frosting
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 7 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_lag()
#> 3. step_epi_ahead()
#> 4. step_naomit()
#> 5. step_naomit()
#> 6. step_training_window()
#> 7. check_enough_data()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
#> 
#> ── Postprocessor ───────────────────────────────────────────────────────────────
#> 
#> 5 Frosting layers.
#> 1. layer_predict()
#> 2. layer_residual_quantiles()
#> 3. layer_add_forecast_date()
#> 4. layer_add_target_date()
#> 5. layer_threshold()
#> 

arx_fcast_epi_workflow(jhu, "death_rate",
  c("case_rate", "death_rate"),
  trainer = quantile_reg(),
  args_list = arx_args_list(quantile_levels = 1:9 / 10)
)
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: quantile_reg()
#> Postprocessor: Frosting
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 7 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_lag()
#> 3. step_epi_ahead()
#> 4. step_naomit()
#> 5. step_naomit()
#> 6. step_training_window()
#> 7. check_enough_data()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> quantile reg Model Specification (regression)
#> 
#> Main Arguments:
#>   quantile_levels = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
#>   method = br
#> 
#> Computational engine: rq 
#> 
#> 
#> ── Postprocessor ───────────────────────────────────────────────────────────────
#> 
#> 6 Frosting layers.
#> 1. layer_predict()
#> 2. layer_quantile_distn()
#> 3. layer_point_from_distn()
#> 4. layer_add_forecast_date()
#> 5. layer_add_target_date()
#> 6. layer_threshold()
#> 
```
