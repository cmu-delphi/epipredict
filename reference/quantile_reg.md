# Quantile regression

`quantile_reg()` generates a quantile regression model *specification*
for the [tidymodels](https://www.tidymodels.org/) framework. Currently,
the only supported engines are "rq", which uses
[`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html). Quantile
regression is also possible by combining
[`parsnip::rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
with the `grf` engine. See
[grf_quantiles](https://cmu-delphi.github.io/epipredict/reference/grf_quantiles.md).

## Usage

``` r
quantile_reg(
  mode = "regression",
  engine = "rq",
  quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
  method = "br"
)
```

## Arguments

- mode:

  A single character string for the type of model. The only possible
  value for this model is "regression".

- engine:

  Character string naming the fitting function. Currently, only "rq" and
  "grf" are supported.

- quantile_levels:

  A scalar or vector of values in (0, 1) to determine which quantiles to
  estimate (default is the set 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95).

- method:

  A fitting method used by
  [`quantreg::rq()`](https://rdrr.io/pkg/quantreg/man/rq.html). See the
  documentation for a list of options.

## See also

[`parsnip::fit.model_spec()`](https://generics.r-lib.org/reference/fit.html),
[`parsnip::set_engine()`](https://parsnip.tidymodels.org/reference/set_engine.html)

## Examples

``` r
library(quantreg)
#> Loading required package: SparseM
tib <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
rq_spec <- quantile_reg(quantile_levels = c(.2, .8)) %>% set_engine("rq")
ff <- rq_spec %>% fit(y ~ ., data = tib)
predict(ff, new_data = tib)
#> # A tibble: 100 × 1
#>        .pred
#>    <qtls(2)>
#>  1   [0.507]
#>  2   [0.234]
#>  3   [0.561]
#>  4  [-0.216]
#>  5  [-0.172]
#>  6  [-0.119]
#>  7  [-0.399]
#>  8  [-0.263]
#>  9  [-0.578]
#> 10   [0.153]
#> # ℹ 90 more rows
```
