# Create short function names

Create short function names

## Usage

``` r
clean_f_name(.f, max_length = 20L)
```

## Arguments

- .f:

  a function, character string, or lambda. For example, `mean`,
  `"mean"`, `~ mean(.x)` or `\(x) mean(x, na.rm = TRUE)`.

- max_length:

  integer determining how long names can be

## Value

a character string of length at most `max_length` that (partially)
describes the function.

## Examples

``` r
clean_f_name(mean)
#> [1] "mean"
clean_f_name("mean")
#> [1] "mean"
clean_f_name(~ mean(.x, na.rm = TRUE))
#> [1] "mean(.x, na.rm = ..."
clean_f_name(\(x) mean(x, na.rm = TRUE))
#> [1] "[ ]{mean(x, na.r...}"
clean_f_name(function(x) mean(x, na.rm = TRUE, trim = 0.2357862))
#> [1] "[ ]{mean(x, na.r...}"
```
