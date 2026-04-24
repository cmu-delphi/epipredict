# Create an epi_workflow

This is a container object that unifies preprocessing, fitting,
prediction, and post-processing for predictive modeling on
epidemiological data. It extends the functionality of a
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)
to handle the typical panel data structures found in this field. This
extension is handled completely internally, and should be invisible to
the user. For all intents and purposes, this operates exactly like a
[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html).
For some `{epipredict}` specific examples, see the [custom epiworkflows
vignette](https://cmu-delphi.github.io/epipredict/dev/articles/custom_epiworkflows.md).

## Usage

``` r
epi_workflow(preprocessor = NULL, spec = NULL, postprocessor = NULL)
```

## Arguments

- preprocessor:

  An optional preprocessor to add to the workflow. One of:

  - A formula, passed on to
    [`add_formula()`](https://workflows.tidymodels.org/reference/add_formula.html).

  - A recipe, passed on to
    [`add_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html).

  - A
    [`workflow_variables()`](https://workflows.tidymodels.org/reference/add_variables.html)
    object, passed on to
    [`add_variables()`](https://workflows.tidymodels.org/reference/add_variables.html).

- spec:

  An optional parsnip model specification to add to the workflow. Passed
  on to
  [`add_model()`](https://workflows.tidymodels.org/reference/add_model.html).

- postprocessor:

  An optional postprocessor to add to the workflow. Currently only
  `frosting` is allowed using,
  [`add_frosting()`](https://cmu-delphi.github.io/epipredict/dev/reference/add_frosting.md).

## Value

A new `epi_workflow` object.

## See also

[`workflows::workflow()`](https://workflows.tidymodels.org/reference/workflow.html)

## Examples

``` r
jhu <- covid_case_death_rates

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_epi_naomit()

wf <- epi_workflow(r, parsnip::linear_reg())

wf
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 5 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_ahead()
#> 3. step_epi_lag()
#> 4. step_naomit()
#> 5. step_naomit()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
#> 
```
