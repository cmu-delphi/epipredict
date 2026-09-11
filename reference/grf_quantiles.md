# Random quantile forests via grf

[`grf::quantile_forest()`](https://rdrr.io/pkg/grf/man/quantile_forest.html)
fits random forests in a way that makes it easy to calculate *quantile*
forests. Currently, this is the only engine provided here, since
quantile regression is the typical use-case.

## Tuning Parameters

This model has 3 tuning parameters:

- `mtry`: \# Randomly Selected Predictors (type: integer, default: see
  below)

- `trees`: \# Trees (type: integer, default: 2000L)

- `min_n`: Minimal Node Size (type: integer, default: 5)

`mtry` depends on the number of columns in the design matrix. The
default in
[`grf::quantile_forest()`](https://rdrr.io/pkg/grf/man/quantile_forest.html)
is `min(ceiling(sqrt(ncol(X)) + 20), ncol(X))`.

For categorical predictors, a one-hot encoding is always used. This
makes splitting efficient, but has implications for the `mtry` choice. A
factor with many levels will become a large number of columns in the
design matrix which means that some of these may be selected frequently
for potential splits. This is different than in other implementations of
random forest. For more details, see [the `grf`
discussion](https://grf-labs.github.io/grf/articles/categorical_inputs.html).

## Translation from parsnip to the original package

    rand_forest(
      mode = "regression", # you must specify the `mode = regression`
      mtry = integer(1),
      trees = integer(1),
      min_n = integer(1)
    ) %>%
      set_engine("grf_quantiles") %>%
      translate()
    #> Random Forest Model Specification (regression)
    #>
    #> Main Arguments:
    #>   mtry = integer(1)
    #>   trees = integer(1)
    #>   min_n = integer(1)
    #>
    #> Computational engine: grf_quantiles
    #>
    #> Model fit template:
    #> grf::quantile_forest(X = missing_arg(), Y = missing_arg(), mtry = min_cols(~integer(1),
    #>     x), num.trees = integer(1), min.node.size = min_rows(~integer(1),
    #>     x), quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
    #>     num.threads = 1L, seed = stats::runif(1, 0, .Machine$integer.max))

## Case weights

Case weights are not supported.

## Examples

``` r
library(grf)
tib <- data.frame(
  y = rnorm(100), x = rnorm(100), z = rnorm(100),
  f = factor(sample(letters[1:3], 100, replace = TRUE))
)
spec <- rand_forest(engine = "grf_quantiles", mode = "regression")
out <- fit(spec, formula = y ~ x + z, data = tib)
predict(out, new_data = tib[1:5, ]) %>%
  pivot_quantiles_wider(.pred)
#> # A tibble: 5 × 7
#>   `0.05`  `0.1` `0.25` `0.5` `0.75` `0.9` `0.95`
#>    <dbl>  <dbl>  <dbl> <dbl>  <dbl> <dbl>  <dbl>
#> 1  -1.91 -1.19  -0.542 0.210  0.943  1.30   1.75
#> 2  -1.91 -1.27  -0.490 0.243  1.07   1.75   2.00
#> 3  -1.24 -1.20  -0.296 0.593  1.24   1.61   2.05
#> 4  -1.18 -0.877 -0.475 0.375  1.09   1.36   1.83
#> 5  -1.49 -0.890 -0.475 0.375  1.08   1.44   1.83

# -- adjusting the desired quantiles

spec <- rand_forest(mode = "regression") %>%
  set_engine(engine = "grf_quantiles", quantiles = c(1:9 / 10))
out <- fit(spec, formula = y ~ x + z, data = tib)
predict(out, new_data = tib[1:5, ]) %>%
  pivot_quantiles_wider(.pred)
#> # A tibble: 5 × 9
#>    `0.1`  `0.2`  `0.3`   `0.4` `0.5` `0.6` `0.7` `0.8` `0.9`
#>    <dbl>  <dbl>  <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 -1.19  -0.698 -0.394 -0.117  0.210 0.634 0.910  1.10  1.31
#> 2 -1.27  -0.682 -0.365 -0.0196 0.243 0.561 0.943  1.30  1.75
#> 3 -1.20  -0.891 -0.100  0.236  0.593 0.657 1.10   1.44  2.05
#> 4 -0.890 -0.811 -0.294  0.243  0.415 0.593 0.996  1.24  1.36
#> 5 -0.890 -0.698 -0.341  0.243  0.375 0.561 0.996  1.24  1.45

# -- a more complicated task

dat <- covid_case_death_rates %>%
  filter(time_value > as.Date("2021-10-01"))
rec <- epi_recipe(dat) %>%
  step_epi_lag(case_rate, death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()
frost <- frosting() %>%
  layer_predict() %>%
  layer_threshold(.pred)
spec <- rand_forest(mode = "regression") %>%
  set_engine(engine = "grf_quantiles", quantiles = c(.25, .5, .75))

ewf <- epi_workflow(rec, spec, frost) %>%
  fit(dat) %>%
  forecast()
ewf %>%
  rename(forecast_date = time_value) %>%
  mutate(target_date = forecast_date + 7L) %>%
  pivot_quantiles_wider(.pred)
#> # A tibble: 56 × 6
#>    geo_value forecast_date target_date `0.25` `0.5` `0.75`
#>    <chr>     <date>        <date>       <dbl> <dbl>  <dbl>
#>  1 ak        2021-12-31    2022-01-07  0.196  0.269  0.420
#>  2 al        2021-12-31    2022-01-07  0.138  0.198  0.298
#>  3 ar        2021-12-31    2022-01-07  0.449  0.511  0.610
#>  4 as        2021-12-31    2022-01-07  0      0      0    
#>  5 az        2021-12-31    2022-01-07  0.518  0.668  0.872
#>  6 ca        2021-12-31    2022-01-07  0.174  0.222  0.267
#>  7 co        2021-12-31    2022-01-07  0.339  0.480  0.617
#>  8 ct        2021-12-31    2022-01-07  0.333  0.420  0.465
#>  9 dc        2021-12-31    2022-01-07  0.0802 0.216  0.504
#> 10 de        2021-12-31    2022-01-07  0.235  0.378  0.530
#> # ℹ 46 more rows
```
