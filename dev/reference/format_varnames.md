# "Format" a character vector of column/variable names for cli interpolation

Designed to give good output if interpolated with cli. Main purpose is
to add backticks around variable names when necessary, and something
other than an empty string if length 0.

## Usage

``` r
format_varnames(x, empty = "*none*")
```

## Arguments

- x:

  `chr`; e.g., `colnames` of some data frame

- empty:

  string; what should be output if `x` is of length 0?

## Value

`chr`
