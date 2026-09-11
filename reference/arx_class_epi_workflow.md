# Create a template `arx_classifier` workflow

This function creates an unfit workflow for use with
[`arx_classifier()`](https://cmu-delphi.github.io/epipredict/reference/arx_classifier.md).
It is useful if you want to make small modifications to that classifier
before fitting and predicting. Supplying a trainer to the function may
alter the returned `epi_workflow` object but can be omitted.

## Usage

``` r
arx_class_epi_workflow(
  epi_data,
  outcome,
  predictors,
  trainer = parsnip::logistic_reg(),
  args_list = arx_class_args_list()
)
```

## Arguments

- epi_data:

  An `epi_df` object

- outcome:

  A character (scalar) specifying the outcome (in the `epi_df`). Note
  that as with
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md),
  this is expected to be real-valued. Conversion of this data to
  unordered classes is handled internally based on the `breaks` argument
  to
  [`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/reference/arx_class_args_list.md).
  If discrete classes are already in the `epi_df`, it is recommended to
  code up a classifier from scratch using
  [`epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/epi_recipe.md).

- predictors:

  A character vector giving column(s) of predictor variables. This
  defaults to the `outcome`. However, if manually specified, only those
  variables specifically mentioned will be used, and the `outcome` will
  not be added.

- trainer:

  A `{parsnip}` model describing the type of estimation. For now, we
  enforce `mode = "classification"`. Typical values are
  [`parsnip::logistic_reg()`](https://parsnip.tidymodels.org/reference/logistic_reg.html)
  or
  [`parsnip::multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html).
  More complicated trainers like
  [`parsnip::naive_Bayes()`](https://parsnip.tidymodels.org/reference/naive_Bayes.html)
  or
  [`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  can also be used. May be `NULL` if you'd like to decide later.

- args_list:

  A list of customization arguments to determine the type of forecasting
  model. See
  [`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/reference/arx_class_args_list.md).

## Value

An unfit `epi_workflow`.

## See also

[`arx_classifier()`](https://cmu-delphi.github.io/epipredict/reference/arx_classifier.md)
[`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/reference/arx_class_args_list.md)

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-11-01"))

arx_class_epi_workflow(jhu, "death_rate", c("case_rate", "death_rate"))
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: logistic_reg()
#> Postprocessor: Frosting
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 8 Recipe steps.
#> 1. step_growth_rate()
#> 2. step_epi_lag()
#> 3. step_epi_lag()
#> 4. step_epi_ahead()
#> 5. step_mutate()
#> 6. step_naomit()
#> 7. step_naomit()
#> 8. step_training_window()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Logistic Regression Model Specification (classification)
#> 
#> Computational engine: glm 
#> 
#> 
#> ── Postprocessor ───────────────────────────────────────────────────────────────
#> 
#> 3 Frosting layers.
#> 1. layer_predict()
#> 2. layer_add_forecast_date()
#> 3. layer_add_target_date()
#> 

arx_class_epi_workflow(
  jhu,
  "death_rate",
  c("case_rate", "death_rate"),
  trainer = multinom_reg(),
  args_list = arx_class_args_list(
    breaks = c(-.05, .1), ahead = 14,
    horizon = 14, method = "linear_reg"
  )
)
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: multinom_reg()
#> Postprocessor: Frosting
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 8 Recipe steps.
#> 1. step_growth_rate()
#> 2. step_epi_lag()
#> 3. step_epi_lag()
#> 4. step_epi_ahead()
#> 5. step_mutate()
#> 6. step_naomit()
#> 7. step_naomit()
#> 8. step_training_window()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Multinomial Regression Model Specification (classification)
#> 
#> Computational engine: nnet 
#> 
#> 
#> ── Postprocessor ───────────────────────────────────────────────────────────────
#> 
#> 3 Frosting layers.
#> 1. layer_predict()
#> 2. layer_add_forecast_date()
#> 3. layer_add_target_date()
#> 
```
