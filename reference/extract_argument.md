# Extract an argument made to a frosting layer or recipe step

Extract an argument made to a frosting layer or recipe step

## Usage

``` r
extract_argument(x, name, arg, ...)
```

## Arguments

- x:

  an epi_workflow, epi_recipe, frosting, step, or layer object

- name:

  the name of the layer

- arg:

  the name of the argument

- ...:

  not used

## Value

An object originally passed as an argument to a layer or step

## Examples

``` r
f <- frosting() %>%
  layer_predict() %>%
  layer_residual_quantiles(symmetrize = FALSE) %>%
  layer_naomit(.pred)

extract_argument(f, "layer_residual_quantiles", "symmetrize")
#> [1] FALSE
```
