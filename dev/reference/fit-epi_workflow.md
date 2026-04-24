# Fit an `epi_workflow` object

This is the [`fit()`](https://generics.r-lib.org/reference/fit.html)
method for an
[`epi_workflow()`](https://cmu-delphi.github.io/epipredict/dev/reference/epi_workflow.md)
object that estimates parameters for a given model from a set of data.
Fitting an
[`epi_workflow()`](https://cmu-delphi.github.io/epipredict/dev/reference/epi_workflow.md)
involves two main steps, which are preprocessing the data and fitting
the underlying parsnip model.

## Usage

``` r
# S3 method for class 'epi_workflow'
fit(object, data, ..., control = workflows::control_workflow())
```

## Arguments

- object:

  an `epi_workflow` object

- data:

  an `epi_df` of predictors and outcomes to use when fitting the
  `epi_workflow`

- ...:

  Not used

- control:

  A
  [`workflows::control_workflow()`](https://workflows.tidymodels.org/reference/control_workflow.html)
  object

## Value

The `epi_workflow` object, updated with a fit parsnip model in the
`object$fit$fit` slot.

## See also

[`workflows::fit-workflow()`](https://workflows.tidymodels.org/reference/fit-workflow.html)

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7)

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
wf
#> 
#> ══ Epi Workflow [trained] ══════════════════════════════════════════════════════
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
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)   lag_0_death_rate   lag_7_death_rate  lag_14_death_rate  
#>           0.32709           -0.01822           -0.02096           -0.05793  
#> 
#> 
```
