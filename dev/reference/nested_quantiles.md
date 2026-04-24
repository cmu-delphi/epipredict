# Turn a vector of quantile distributions into a list-col

**\[deprecated\]**

## Usage

``` r
nested_quantiles(x)
```

## Arguments

- x:

  a `distribution` containing `dist_quantiles`

## Value

a list-col

## Details

This function is deprecated. The recommended alternative is
[`hardhat::quantile_pred()`](https://hardhat.tidymodels.org/reference/quantile_pred.html)
with
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)

## Examples

``` r
pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
nested_quantiles(pred_quantile)
#> Warning: `nested_quantiles()` was deprecated in epipredict 0.1.11.
#> ℹ Please use `hardhat::quantile_pred()` instead.
#> # A tibble: 5 × 1
#>   data            
#>   <list>          
#> 1 <tibble [4 × 2]>
#> 2 <tibble [4 × 2]>
#> 3 <tibble [4 × 2]>
#> 4 <tibble [4 × 2]>
#> 5 <tibble [4 × 2]>

pred_quantile %>%
  as_tibble() %>%
  tidyr::nest(.by = .row) %>%
  dplyr::select(-.row)
#> # A tibble: 5 × 1
#>   data            
#>   <list>          
#> 1 <tibble [4 × 2]>
#> 2 <tibble [4 × 2]>
#> 3 <tibble [4 × 2]>
#> 4 <tibble [4 × 2]>
#> 5 <tibble [4 × 2]>
```
