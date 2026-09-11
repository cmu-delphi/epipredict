# Tidy the result of a frosting object

`tidy` will return a data frame that contains information regarding a
frosting or operation within the frosting (when a `tidy` method for the
operation exists). Note that this is a modified version of the `tidy`
method for a recipe.

## Usage

``` r
# S3 method for class 'frosting'
tidy(x, number = NA, id = NA, ...)
```

## Arguments

- x:

  A `frosting` or `layer` object

- number:

  An integer or `NA`. If missing, and `id` is not provided, the return
  value is a list of the operations in the frosting. If a number is
  given, a `tidy` method is executed for that operation in the frosting
  (if it exists). `number` must not be provided if `id` is.

- id:

  A character string or `NA`. If missing and `number` is not provided,
  the return value is a list of the operations in the frosting. If a
  character string is given, a `tidy` method is executed for that
  operation in the frosting (if it exists). `id` must not be provided if
  `number` is.

- ...:

  Not currently used.

## Value

A tibble with columns that vary depending on what `tidy` method is
executed. When `number`, and `id` are `NA`, a tibble with columns
`number` (the operation iteration), `operation` ("layer"), `type` (the
method, e.g. "predict", "naomit"), and a character column `id`.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#> Error: object 'r' not found
latest <- get_test_data(recipe = r, x = jhu)
#> Error in get_test_data(recipe = r, x = jhu): `x` must be an `epi_df`.

f <- frosting() %>%
  layer_predict() %>%
  layer_naomit(.pred)

tidy(f)
#> # A tibble: 2 × 4
#>   number operation type    id                   
#>    <int> <chr>     <chr>   <chr>                
#> 1      1 layer     predict predict_default_FMUfJ
#> 2      2 layer     naomit  naomit_jNB0j         
```
