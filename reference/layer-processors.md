# Extract, validate, or detect layers of frosting

These functions are mainly internal. They can access and validate
different layers of `frosting`.

## Usage

``` r
extract_layers(x, ...)

# S3 method for class 'frosting'
extract_layers(x, ...)

# S3 method for class 'workflow'
extract_layers(x, ...)

is_layer(x)

validate_layer(x, ..., arg = rlang::caller_arg(x), call = caller_env())

detect_layer(x, name, ...)

# S3 method for class 'frosting'
detect_layer(x, name, ...)

# S3 method for class 'workflow'
detect_layer(x, name, ...)
```

## Arguments

- x:

  an `epi_workflow`, `frosting`, or `layer` object

- ...:

  additional arguments for possible future methods

- arg:

  the name of the input (for error reporting)

- call:

  the environment (for error reporting)

- name:

  a layer name to detect

## Value

A logical for the validators/detectors or a list of layers for the
extractors

## Examples

``` r

f <- frosting() %>% layer_predict()
wf <- epi_workflow(postprocessor = f)

is_layer(extract_layers(f)[[1]])
#> [1] FALSE
detect_layer(f, "layer_predict")
#> [1] TRUE
detect_layer(wf, "layer_predict")
#> [1] TRUE

extract_layers(f)
#> [[1]]
#> Creating predictions: "<calculated>"
#> 
extract_layers(wf)
#> [[1]]
#> Creating predictions: "<calculated>"
#> 
```
