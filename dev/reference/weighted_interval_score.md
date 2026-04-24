# Compute weighted interval score

Weighted interval score (WIS), a well-known quantile-based approximation
of the commonly-used continuous ranked probability score (CRPS). WIS is
a proper score, and can be thought of as a distributional generalization
of absolute error. For example, see [Bracher et al.
(2020)](https://arxiv.org/abs/2005.12881) for discussion in the context
of COVID-19 forecasting.

## Usage

``` r
weighted_interval_score(
  x,
  actual,
  quantile_levels = NULL,
  na_handling = c("impute", "drop", "propagate", "fail"),
  ...
)
```

## Arguments

- x:

  A vector of class `quantile_pred`.

- actual:

  double. Actual value(s)

- quantile_levels:

  probabilities. If specified, the score will be computed at this set of
  levels. Otherwise, those present in `x` will be used.

- na_handling:

  character. Determines missing values are handled. For `"impute"`,
  missing values will be calculated if possible using the available
  quantiles. For `"drop"`, explicitly missing values are ignored in the
  calculation of the score, but implicitly missing values are imputed if
  possible. For `"propogate"`, the resulting score will be `NA` if any
  missing values exist. Finally, if `quantile_levels` is specified,
  `"fail"` will result in the score being `NA` when any required
  quantile levels (implicit or explicit) do not have corresponding
  values.

- ...:

  not used

## Value

a vector of nonnegative scores.

## Examples

``` r
quantile_levels <- c(.2, .4, .6, .8)
predq1 <- 1:4 #
predq2 <- 8:11
dstn <- quantile_pred(rbind(predq1, predq2), quantile_levels)
actual <- c(3.3, 7.1)
weighted_interval_score(dstn, actual)
#> [1] 0.65 1.90
weighted_interval_score(dstn, actual, c(.25, .5, .75))
#> [1] 0.6833333 1.9833333

# Missing value behaviours
dstn <- quantile_pred(matrix(c(1, 2, NA, 4), nrow = 1), 1:4 / 5)
weighted_interval_score(dstn, 2.5)
#> [1] 0.5
weighted_interval_score(dstn, 2.5, 1:9 / 10)
#> [1] 0.455656
weighted_interval_score(dstn, 2.5, 1:9 / 10, na_handling = "drop")
#> [1] 0.462613
weighted_interval_score(dstn, 2.5, na_handling = "propagate")
#> [1] NA
weighted_interval_score(
  quantile_pred(matrix(1:4, nrow = 1), 1:4 / 5),
  actual = 2.5,
  quantile_levels = 1:9 / 10,
  na_handling = "fail"
)
#> [1] NA


# Using some actual forecasts --------
library(dplyr)
training <- covid_case_death_rates %>%
  filter(time_value >= "2021-10-01", time_value <= "2021-12-01")
preds <- flatline_forecaster(
  training, "death_rate",
  flatline_args_list(quantile_levels = c(.01, .025, 1:19 / 20, .975, .99))
)$predictions
actuals <- covid_case_death_rates %>%
  filter(time_value == as.Date("2021-12-01") + 7) %>%
  select(geo_value, time_value, actual = death_rate)
preds <- left_join(preds, actuals,
  by = c("target_date" = "time_value", "geo_value")
) %>%
  mutate(wis = weighted_interval_score(.pred_distn, actual))
preds
#> # A tibble: 56 × 7
#>    geo_value .pred .pred_distn forecast_date target_date actual    wis
#>    <chr>     <dbl>  <qtls(23)> <date>        <date>       <dbl>  <dbl>
#>  1 ak        0.217     [0.217] 2021-12-01    2021-12-08  0.0988 0.0673
#>  2 al        0.119     [0.119] 2021-12-01    2021-12-08  0.174  0.0364
#>  3 ar        0.207     [0.207] 2021-12-01    2021-12-08  0.514  0.196 
#>  4 as        0             [0] 2021-12-01    2021-12-08  0      0.0145
#>  5 az        0.485     [0.485] 2021-12-01    2021-12-08  0.826  0.223 
#>  6 ca        0.169     [0.169] 2021-12-01    2021-12-08  0.185  0.0278
#>  7 co        0.509     [0.509] 2021-12-01    2021-12-08  0.534  0.0313
#>  8 ct        0.177     [0.177] 2021-12-01    2021-12-08  0.149  0.0301
#>  9 dc        0             [0] 2021-12-01    2021-12-08  0.0200 0.0166
#> 10 de        0.217     [0.217] 2021-12-01    2021-12-08  0.391  0.101 
#> # ℹ 46 more rows
```
