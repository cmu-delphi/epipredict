# Adjust a step in an `epi_workflow` or `epi_recipe`

Make a parameter adjustment to a step in either an `epi_workflow` or
`epi_recipe` object.

## Usage

``` r
adjust_epi_recipe(
  x,
  which_step,
  ...,
  blueprint = default_epi_recipe_blueprint()
)

# S3 method for class 'epi_workflow'
adjust_epi_recipe(
  x,
  which_step,
  ...,
  blueprint = default_epi_recipe_blueprint()
)

# S3 method for class 'epi_recipe'
adjust_epi_recipe(
  x,
  which_step,
  ...,
  blueprint = default_epi_recipe_blueprint()
)
```

## Arguments

- x:

  A `epi_workflow` or `epi_recipe` object

- which_step:

  the number or name of the step to adjust

- ...:

  Used to input a parameter adjustment

- blueprint:

  A hardhat blueprint used for fine tuning the preprocessing.

## Value

`x`, updated with the adjustment to the specified `epi_recipe` step.

## Details

This function can either adjust a step in a `epi_recipe` object or a
step from a `epi_recipe` object in an `epi_workflow`. The step to be
adjusted is indicated by either the step number or name (if a name is
used, it must be unique). In either case, the argument name and update
value must be inputted as `...`. See the examples below for brief
illustrations of the different types of updates.

## Examples

``` r
library(workflows)
#> 
#> Attaching package: ‘workflows’
#> The following objects are masked from ‘package:epipredict’:
#> 
#>     add_model, remove_model, update_model

jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- jhu %>%
  filter(time_value >= max(time_value) - 14)

# Adjust `step_epi_ahead` to have an ahead value of 14
# in the `epi_workflow`
# Option 1. Using the step number:
wf2 <- wf %>% adjust_epi_recipe(which_step = 2, ahead = 14)
extract_preprocessor(wf2)
#> 
#> ── Epi Recipe ──────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations 
#> 1. Lagging: death_rate by 0, 7, 14
#> 2. Leading: death_rate by 14
#> 3. • Removing rows with NA values in: all_predictors()
#> 4. • Removing rows with NA values in: all_outcomes()
# Option 2. Using the step name:
wf3 <- wf %>% adjust_epi_recipe(which_step = "step_epi_ahead", ahead = 14)
extract_preprocessor(wf3)
#> 
#> ── Epi Recipe ──────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations 
#> 1. Lagging: death_rate by 0, 7, 14
#> 2. Leading: death_rate by 14
#> 3. • Removing rows with NA values in: all_predictors()
#> 4. • Removing rows with NA values in: all_outcomes()

# Adjust `step_epi_ahead` to have an ahead value of 14
# in the `epi_recipe`
# Option 1. Using the step number
r2 <- r %>% adjust_epi_recipe(which_step = 2, ahead = 14)
r2
#> 
#> ── Epi Recipe ──────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations 
#> 1. Lagging: death_rate by 0, 7, 14
#> 2. Leading: death_rate by 14
#> 3. • Removing rows with NA values in: all_predictors()
#> 4. • Removing rows with NA values in: all_outcomes()
# Option 2. Using the step name
r3 <- r %>% adjust_epi_recipe(which_step = "step_epi_ahead", ahead = 14)
r3
#> 
#> ── Epi Recipe ──────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations 
#> 1. Lagging: death_rate by 0, 7, 14
#> 2. Leading: death_rate by 14
#> 3. • Removing rows with NA values in: all_predictors()
#> 4. • Removing rows with NA values in: all_outcomes()
```
