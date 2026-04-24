# Add/remove/update the `epi_recipe` of an `epi_workflow`

- `add_recipe()` specifies the terms of the model and any preprocessing
  that is required through the usage of a recipe.

- `remove_recipe()` removes the recipe as well as any downstream objects

- `update_recipe()` first removes the recipe, then replaces the previous
  recipe with the new one.

## Usage

``` r
add_epi_recipe(x, recipe, ..., blueprint = default_epi_recipe_blueprint())

remove_epi_recipe(x)

update_epi_recipe(x, recipe, ..., blueprint = default_epi_recipe_blueprint())
```

## Arguments

- x:

  A `workflow` or `epi_workflow`

- recipe:

  An epi recipe or recipe

- ...:

  Not used

- blueprint:

  A hardhat blueprint used for fine tuning the preprocessing

  [`default_epi_recipe_blueprint()`](https://cmu-delphi.github.io/epipredict/dev/reference/new_epi_recipe_blueprint.md)
  is used.

  Note that preprocessing done here is separate from preprocessing that
  might be done automatically by the underlying model.

## Value

`x`, updated with a new recipe preprocessor.

## Details

`add_epi_recipe()` has the same behaviour as
[`workflows::add_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html)
but sets a different default blueprint to automatically handle
[`epiprocess::epi_df()`](https://cmu-delphi.github.io/epiprocess/reference/epi_df.html)
data.

## See also

[`workflows::add_recipe()`](https://workflows.tidymodels.org/reference/add_recipe.html)

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-08-01")

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_epi_naomit()

workflow <- epi_workflow() %>%
  add_epi_recipe(r)

workflow
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: None
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
#> 

r2 <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7)

workflow <- update_epi_recipe(workflow, r2)

workflow <- remove_epi_recipe(workflow)

workflow
#> 
#> ══ Epi Workflow ════════════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: None
#> Postprocessor: None
#> 
#> 
```
