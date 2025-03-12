
<!-- README.md is generated from README.Rmd. Please edit that file -->

# epipredict

<!-- badges: start -->

[![R-CMD-check](https://github.com/cmu-delphi/epipredict/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmu-delphi/epipredict/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

**Note:** This package is currently in development and may not work as
expected. Please file bug reports as issues in this repo, and we will do
our best to address them quickly.

## Installation

To install (unless you’re making changes to the package, use the stable
version):

``` r
# Stable version
pak::pkg_install("cmu-delphi/epipredict@main")

# Dev version
pak::pkg_install("cmu-delphi/epipredict@dev")
```

## Documentation

You can view documentation for the `main` branch at
<https://cmu-delphi.github.io/epipredict>.

## Goals for `epipredict`

**We hope to provide:**

1.  A set of basic, easy-to-use forecasters that work out of the box.
    You should be able to do a reasonably limited amount of
    customization on them. For the basic forecasters, we currently
    provide:
    - Baseline flatline forecaster
    - Autoregressive forecaster
    - Autoregressive classifier
    - CDC FluSight flatline forecaster
2.  A framework for creating custom forecasters out of modular
    components. There are four types of components:
    - Preprocessor: do things to the data before model training
    - Trainer: train a model on data, resulting in a fitted model object
    - Predictor: make predictions, using a fitted model object
    - Postprocessor: do things to the predictions before returning

**Target audiences:**

- Basic. Has data, calls forecaster with default arguments.
- Intermediate. Wants to examine changes to the arguments, take
  advantage of built in flexibility.
- Advanced. Wants to write their own forecasters. Maybe willing to build
  up from some components.

The Advanced user should find their task to be relatively easy. Examples
of these tasks are illustrated in the [vignettes and
articles](https://cmu-delphi.github.io/epipredict).

See also the (in progress) [Forecasting
Book](https://cmu-delphi.github.io/delphi-tooling-book/).

## Intermediate example

The package comes with some built-in historical data for illustration,
but up-to-date versions of this could be downloaded with the
[`{epidatr}` package](https://cmu-delphi.github.io/epidatr/) and
processed using
[`{epiprocess}`](https://cmu-delphi.github.io/epiprocess/).[^1]

``` r
library(epipredict)
covid_case_death_rates
#> An `epi_df` object, 20,496 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2023-03-10
#> 
#> # A tibble: 20,496 × 4
#>    geo_value time_value case_rate death_rate
#>  * <chr>     <date>         <dbl>      <dbl>
#>  1 ak        2020-12-31      35.9      0.158
#>  2 al        2020-12-31      65.1      0.438
#>  3 ar        2020-12-31      66.0      1.27 
#>  4 as        2020-12-31       0        0    
#>  5 az        2020-12-31      76.8      1.10 
#>  6 ca        2020-12-31      95.9      0.755
#>  7 co        2020-12-31      37.8      0.376
#>  8 ct        2020-12-31      52.1      0.819
#>  9 dc        2020-12-31      31.0      0.601
#> 10 de        2020-12-31      64.3      0.912
#> # ℹ 20,486 more rows
```

To create and train a simple auto-regressive forecaster to predict the
death rate two weeks into the future using past (lagged) deaths and
cases, we could use the following function.

``` r
two_week_ahead <- arx_forecaster(
  covid_case_death_rates,
  outcome = "death_rate",
  predictors = c("case_rate", "death_rate"),
  args_list = arx_args_list(
    lags = list(c(0, 1, 2, 3, 7, 14), c(0, 7, 14)),
    ahead = 14
  )
)
two_week_ahead
#> ══ A basic forecaster of type ARX Forecaster ═══════════════════════════════
#> 
#> This forecaster was fit on 2025-02-11 12:32:56.
#> 
#> Training data was an <epi_df> with:
#> • Geography: state,
#> • Time type: day,
#> • Using data up-to-date as of: 2023-03-10.
#> • With the last data available on 2021-12-31
#> 
#> ── Predictions ─────────────────────────────────────────────────────────────
#> 
#> A total of 56 predictions are available for
#> • 56 unique geographic regions,
#> • At forecast date: 2021-12-31,
#> • For target date: 2022-01-14,
#> 
```

In this case, we have used a number of different lags for the case rate,
while only using 3 weekly lags for the death rate (as predictors). The
result is both a fitted model object which could be used any time in the
future to create different forecasts, as well as a set of predicted
values (and prediction intervals) for each location 14 days after the
last available time value in the data.

``` r
two_week_ahead$epi_workflow
#> 
#> ══ Epi Workflow [trained] ══════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: Frosting
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────
#> 
#> 6 Recipe steps.
#> 1. step_epi_lag()
#> 2. step_epi_lag()
#> 3. step_epi_ahead()
#> 4. step_naomit()
#> 5. step_naomit()
#> 6. step_training_window()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)    lag_0_case_rate    lag_1_case_rate    lag_2_case_rate  
#>        -0.0071026          0.0040340          0.0007863          0.0003699  
#>   lag_3_case_rate    lag_7_case_rate   lag_14_case_rate   lag_0_death_rate  
#>         0.0012887          0.0011980          0.0002527          0.1348573  
#>  lag_7_death_rate  lag_14_death_rate  
#>         0.1479274          0.1067074
#> 
#> ── Postprocessor ───────────────────────────────────────────────────────────
#> 
#> 5 Frosting layers.
#> 1. layer_predict()
#> 2. layer_residual_quantiles()
#> 3. layer_add_forecast_date()
#> 4. layer_add_target_date()
#> 5. layer_threshold()
#> 
```

The fitted model here involved preprocessing the data to appropriately
generate lagged predictors, estimating a linear model with `stats::lm()`
and then postprocessing the results to be meaningful for epidemiological
tasks. We can also examine the predictions.

``` r
two_week_ahead$predictions
#> # A tibble: 56 × 5
#>    geo_value .pred        .pred_distn forecast_date target_date
#>    <chr>     <dbl>             <dist> <date>        <date>     
#>  1 ak        0.450 quantiles(0.45)[7] 2021-12-31    2022-01-14 
#>  2 al        0.602  quantiles(0.6)[7] 2021-12-31    2022-01-14 
#>  3 ar        0.694 quantiles(0.69)[7] 2021-12-31    2022-01-14 
#>  4 as        0        quantiles(0)[7] 2021-12-31    2022-01-14 
#>  5 az        0.699  quantiles(0.7)[7] 2021-12-31    2022-01-14 
#>  6 ca        0.592 quantiles(0.59)[7] 2021-12-31    2022-01-14 
#>  7 co        1.47  quantiles(1.47)[7] 2021-12-31    2022-01-14 
#>  8 ct        1.08  quantiles(1.08)[7] 2021-12-31    2022-01-14 
#>  9 dc        2.14  quantiles(2.14)[7] 2021-12-31    2022-01-14 
#> 10 de        1.13  quantiles(1.13)[7] 2021-12-31    2022-01-14 
#> # ℹ 46 more rows
```

The results above show a distributional forecast produced using data
through the end of 2021 for the 14th of January 2022. A prediction for
the death rate per 100K inhabitants is available for every state
(`geo_value`) along with a 90% predictive interval.

[^1]: Other epidemiological signals for non-Covid related illnesses are
    also available with
    [`{epidatr}`](https://github.com/cmu-delphi/epidatr) which
    interfaces directly to Delphi’s [Epidata
    API](https://cmu-delphi.github.io/delphi-epidata/)
