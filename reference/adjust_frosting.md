# Adjust a layer in an `epi_workflow` or `frosting`

Make a parameter adjustment to a layer in either an `epi_workflow` or
`frosting` object.

## Usage

``` r
adjust_frosting(x, which_layer, ...)

# S3 method for class 'epi_workflow'
adjust_frosting(x, which_layer, ...)

# S3 method for class 'frosting'
adjust_frosting(x, which_layer, ...)
```

## Arguments

- x:

  An `epi_workflow` or `frosting` object

- which_layer:

  the number or name of the layer to adjust

- ...:

  Used to input a parameter adjustment

## Value

`x`, updated with the adjustment to the specified `frosting` layer.

## Details

This function can either adjust a layer in a `frosting` object or a
layer from a `frosting` object in an `epi_workflow`. The layer to be
adjusted is indicated by either the layer number or name (if a name is
used, it must be unique). In either case, the argument name and update
value must be inputted as `...`. See the examples below for brief
illustrations of the different types of updates.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)

# in the frosting from the workflow
f1 <- frosting() %>%
  layer_predict() %>%
  layer_threshold(.pred)

wf2 <- wf %>% add_frosting(f1)

# Adjust `layer_threshold` to have an upper bound of 1
# in the `epi_workflow`
# Option 1. Using the layer number:
wf2 <- wf2 %>% adjust_frosting(which_layer = 2, upper = 1)
extract_frosting(wf2)
#> 
#> ── Frosting ────────────────────────────────────────────────────────────────────
#> 
#> ── Layers 
#> 1. Creating predictions: "<calculated>"
#> 2. Thresholding predictions: .pred to [0, 1]
# Option 2. Using the layer name:
wf3 <- wf2 %>% adjust_frosting(which_layer = "layer_threshold", upper = 1)
extract_frosting(wf3)
#> 
#> ── Frosting ────────────────────────────────────────────────────────────────────
#> 
#> ── Layers 
#> 1. Creating predictions: "<calculated>"
#> 2. Thresholding predictions: .pred to [0, 1]

# Adjust `layer_threshold` to have an upper bound of 5
# in the `frosting` object
# Option 1. Using the layer number:
f2 <- f1 %>% adjust_frosting(which_layer = 2, upper = 5)
f2
#> 
#> ── Frosting ────────────────────────────────────────────────────────────────────
#> 
#> ── Layers 
#> 1. Creating predictions: "<calculated>"
#> 2. Thresholding predictions: .pred to [0, 5]
# Option 2. Using the layer name
f3 <- f1 %>% adjust_frosting(which_layer = "layer_threshold", upper = 5)
f3
#> 
#> ── Frosting ────────────────────────────────────────────────────────────────────
#> 
#> ── Layers 
#> 1. Creating predictions: "<calculated>"
#> 2. Thresholding predictions: .pred to [0, 5]
```
