# Add a model to an `epi_workflow`

Add a model to an `epi_workflow`

## Usage

``` r
Add_model(x, spec, ..., formula = NULL)

Remove_model(x)

Update_model(x, spec, ..., formula = NULL)

# S3 method for class 'epi_workflow'
Add_model(x, spec, ..., formula = NULL)

# S3 method for class 'epi_workflow'
Remove_model(x)

# S3 method for class 'epi_workflow'
Update_model(x, spec, ..., formula = NULL)

# S3 method for class 'workflow'
Add_model(x, spec, ..., formula = NULL)

# S3 method for class 'workflow'
Remove_model(x)

# S3 method for class 'workflow'
Update_model(x, spec, ..., formula = NULL)

add_model(x, spec, ..., formula = NULL)

remove_model(x)

update_model(x, spec, ..., formula = NULL)
```

## Arguments

- x:

  An `epi_workflow`.

- spec:

  A parsnip model specification.

- ...:

  Not used.

- formula:

  An optional formula override to specify the terms of the model.
  Typically, the terms are extracted from the formula or recipe
  preprocessing methods. However, some models (like survival and
  bayesian models) use the formula not to preprocess, but to specify the
  structure of the model. In those cases, a formula specifying the model
  structure must be passed unchanged into the model call itself. This
  argument is used for those purposes.

## Value

`x`, updated with a new, updated, or removed model.

## Details

Has the same behaviour as
[`workflows::add_model()`](https://workflows.tidymodels.org/reference/add_model.html)
but also ensures that the returned object is an `epi_workflow`.

This family is called `Add_*` / `Update_*` / `Remove_*` to avoid masking
the related functions in `{workflows}`. We also provide aliases with the
lower-case names. However, in the event that `{workflows}` is loaded
after `{epipredict}`, these may fail to function properly.

## See also

[`workflows::add_model()`](https://workflows.tidymodels.org/reference/add_model.html)

- `Add_model()` adds a parsnip model to the `epi_workflow`.

- `Remove_model()` removes the model specification as well as any fitted
  model object. Any extra formulas are also removed.

- `Update_model()` first removes the model then adds the new
  specification to the workflow.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7)

rf_model <- rand_forest(mode = "regression")

wf <- epi_workflow(r)

wf <- wf %>% Add_model(rf_model)
wf
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: rand_forest()
#> Postprocessor: None
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 2 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_ahead()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Random Forest Model Specification (regression)
#> 
#> Computational engine: ranger 
#> 
#> 

lm_model <- linear_reg()

wf <- Update_model(wf, lm_model)
wf
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 2 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_ahead()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
#> 

wf <- Remove_model(wf)
wf
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: None
#> Postprocessor: None
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 2 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_ahead()
#> 
#> 
```
