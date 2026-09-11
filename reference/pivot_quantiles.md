# Pivot a column containing `quantile_pred` to explicit rows or columns

Both functions expand a column of `quantile_pred`s into the separate
quantiles. Since each consists of a set of names (quantiles) and values,
these operate analogously with `pivot_wider` and `pivot_longer`.

## Usage

``` r
pivot_quantiles_longer(.data, ...)

pivot_quantiles_wider(.data, ...)
```

## Arguments

- .data:

  A data frame, or a data frame extension such as a tibble or epi_df.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One unquoted expressions separated by commas. Variable names can be
  used as if they were positions in the data frame. Note that only one
  variable can be selected for this operation.

## Value

An object of the same class as `.data`.

## Details

`piot_quantiles_wider` creates a new column for each `quantile_level`,
with the values as the corresponding quantile values. When pivoting
multiple columns, the original column name will be used as a prefix.

Similarly, `pivot_quantiles_longer` assigns the selected columns
`quantile_level`s in one column and the `value`s in another. If multiple
columns are selected, these will be prefixed with the column name.

## Examples

``` r
d1 <- quantile_pred(rbind(1:3, 2:4), 1:3 / 4)
d2 <- quantile_pred(rbind(2:4, 3:5), 2:4 / 5)
tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)

pivot_quantiles_longer(tib, "d1")
#> # A tibble: 6 × 4
#>   g            d2 d1_value d1_quantile_level
#>   <chr> <qtls(3)>    <int>             <dbl>
#> 1 a         [2.5]        1              0.25
#> 2 a         [2.5]        2              0.5 
#> 3 a         [2.5]        3              0.75
#> 4 b         [3.5]        2              0.25
#> 5 b         [3.5]        3              0.5 
#> 6 b         [3.5]        4              0.75
pivot_quantiles_longer(tib, dplyr::ends_with("1"))
#> # A tibble: 6 × 4
#>   g            d2 d1_value d1_quantile_level
#>   <chr> <qtls(3)>    <int>             <dbl>
#> 1 a         [2.5]        1              0.25
#> 2 a         [2.5]        2              0.5 
#> 3 a         [2.5]        3              0.75
#> 4 b         [3.5]        2              0.25
#> 5 b         [3.5]        3              0.5 
#> 6 b         [3.5]        4              0.75
pivot_quantiles_longer(tib, d2)
#> # A tibble: 6 × 4
#>   g            d1 d2_value d2_quantile_level
#>   <chr> <qtls(3)>    <int>             <dbl>
#> 1 a           [2]        2               0.4
#> 2 a           [2]        3               0.6
#> 3 a           [2]        4               0.8
#> 4 b           [3]        3               0.4
#> 5 b           [3]        4               0.6
#> 6 b           [3]        5               0.8

pivot_quantiles_wider(tib, "d1")
#> # A tibble: 2 × 5
#>   g            d2 `0.25` `0.5` `0.75`
#>   <chr> <qtls(3)>  <int> <int>  <int>
#> 1 a         [2.5]      1     2      3
#> 2 b         [3.5]      2     3      4
pivot_quantiles_wider(tib, dplyr::ends_with("2"))
#> # A tibble: 2 × 5
#>   g            d1 `0.4` `0.6` `0.8`
#>   <chr> <qtls(3)> <int> <int> <int>
#> 1 a           [2]     2     3     4
#> 2 b           [3]     3     4     5
pivot_quantiles_wider(tib, d2)
#> # A tibble: 2 × 5
#>   g            d1 `0.4` `0.6` `0.8`
#>   <chr> <qtls(3)> <int> <int> <int>
#> 1 a           [2]     2     3     4
#> 2 b           [3]     3     4     5
```
