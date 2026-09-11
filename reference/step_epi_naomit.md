# Unified NA omission wrapper function for recipes

Unified NA omission wrapper function for recipes

## Usage

``` r
step_epi_naomit(recipe)
```

## Arguments

- recipe:

  Recipe to be used for omission steps

## Value

Omits NA's from both predictors and outcomes at training time to fit the
model. Also only omits associated predictors and not outcomes at
prediction time due to lack of response and avoidance of data loss.
Given a `recipe`, this step is literally equivalent to

     recipe %>%
       recipes::step_naomit(all_predictors(), skip = FALSE) %>%
       recipes::step_naomit(all_outcomes(), skip = TRUE)

## Examples

``` r
covid_case_death_rates %>%
  epi_recipe() %>%
  step_epi_naomit()
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
```
