# `layer` sets the class of the `layer`

`layer` sets the class of the `layer`

## Usage

``` r
layer(subclass, ..., .prefix = "layer_")
```

## Arguments

- subclass:

  A character string for the resulting class. For example, if
  `subclass = "blah"` the layer object that is returned has class
  `layer_blah`.

- ...:

  All arguments to the operator that should be returned.

- .prefix:

  Prefix to the subclass created.

## Value

An updated layer with the new class
