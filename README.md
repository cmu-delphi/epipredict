
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
forecast_date <- as.Date("2021-08-01")
```

Below the fold, we construct this dataset as an `epiprocess::epi_df`
from JHU data.

<details>
<summary>
Creating the dataset using `{epidatr}` and `{epiprocess}`
</summary>

This dataset can be found in the package as \<TODO DOESN’T EXIST\>; we
demonstrate some of the typically ubiquitous cleaning operations needed
to be able to forecast. First we pull both jhu-csse cases and deaths
from [`{epidatr}`](https://cmu-delphi.github.io/epidatr/) package:

``` r
cases <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20220101),
  geo_values = "*") |>
  select(geo_value, time_value, case_rate = value)

deaths <- pub_covidcast(
  source = "jhu-csse",
  signals = "deaths_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20220101),
  geo_values = "*") |>
  select(geo_value, time_value, death_rate = value)
cases_deaths <-
  full_join(cases, deaths, by = c("time_value", "geo_value")) |>
  as_epi_df(as_of = as.Date("2022-01-01"))
plot_locations <- c("ca", "ma", "ny", "tx")
# plotting the data as it was downloaded
cases_deaths |>
  filter(geo_value %in% plot_locations) |>
  pivot_longer(cols = c("case_rate", "death_rate"), names_to = "source") |>
  ggplot(aes(x = time_value, y = value)) +
  geom_line() +
  facet_grid(source ~ geo_value, scale = "free") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="man/figures/README-case_death-1.png" width="90%" style="display: block; margin: auto;" />
As with basically any dataset, there is some cleaning that we will need
to do to make it actually usable; we’ll use some utilities from
[`{epiprocess}`](https://cmu-delphi.github.io/epiprocess/) for this.
First, to eliminate some of the noise coming from daily reporting, we do
7 day averaging over a trailing window[^1]:

``` r
cases_deaths <-
  cases_deaths |> 
  group_by(geo_value) |>
  epi_slide(
    cases_7dav = mean(case_rate, na.rm = TRUE),
    death_rate_7dav = mean(death_rate, na.rm = TRUE),
    .window_size = 7
  ) |>
  ungroup() |>
  mutate(case_rate = NULL, death_rate = NULL) |>
  rename(case_rate = cases_7dav, death_rate = death_rate_7dav)
```

Then trimming outliers, most especially negative values:

``` r
cases_deaths <-
  cases_deaths |>
  group_by(geo_value) |>
  mutate(
    outlr_death_rate = detect_outlr_rm(time_value, death_rate, detect_negatives = TRUE),
    outlr_case_rate = detect_outlr_rm(time_value, case_rate, detect_negatives = TRUE)
  ) |>
  unnest(cols = starts_with("outlr"), names_sep = "_") |>
  ungroup() |>
  mutate(
    death_rate = outlr_death_rate_replacement,
    case_rate = outlr_case_rate_replacement) |>
  select(geo_value, time_value, case_rate, death_rate)
cases_deaths
#> An `epi_df` object, 32,480 x 4 with metadata:
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
forecast_date_label <-
  tibble(
    geo_value = rep(plot_locations, 2),
    source = c(rep("case_rate",4), rep("death_rate", 4)),
    dates = rep(forecast_date - 7*2, 2 * length(plot_locations)),
    heights = c(rep(150, 4), rep(1.0, 4))
  )
processed_data_plot <-
  cases_deaths |>
  filter(geo_value %in% plot_locations) |>
  pivot_longer(cols = c("case_rate", "death_rate"), names_to = "source") |>
  ggplot(aes(x = time_value, y = value)) +
  geom_line() +
  facet_grid(source ~ geo_value, scale = "free") +
  geom_vline(aes(xintercept = forecast_date)) +
  geom_text(
    data = forecast_date_label, aes(x=dates, label = "forecast\ndate", y = heights), size = 3, hjust = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

</details>

<img src="man/figures/README-show-processed-data-1.png" width="90%" style="display: block; margin: auto;" />

To make a forecast, we will use a “canned” simple auto-regressive
forecaster to predict the death rate four weeks into the future using
lagged[^2] deaths and cases

``` r
four_week_ahead <- arx_forecaster(
  cases_deaths |> filter(time_value <= forecast_date),
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

In this case, we have used 0-3 days, a week, and two week lags for the
case rate, while using only zero, one and two weekly lags for the death
rate (as predictors). The result `four_week_ahead` is both a fitted
model object which could be used any time in the future to create
different forecasts, as well as a set of predicted values (and
prediction intervals) for each location 28 days after the forecast date.
Plotting the prediction intervals on our subset above[^3]:

<details>
<summary>
Plot
</summary>

This is the same kind of plot as `processed_data_plot` above, but with
the past data narrowed somewhat

``` r
narrow_data_plot <-
  cases_deaths |>
  filter(time_value > "2021-04-01") |>
  filter(geo_value %in% plot_locations) |>
  pivot_longer(cols = c("case_rate", "death_rate"), names_to = "source") |>
  ggplot(aes(x = time_value, y = value)) +
  geom_line() +
  facet_grid(source ~ geo_value, scale = "free") +
  geom_vline(aes(xintercept = forecast_date)) +
  geom_text(
    data = forecast_date_label, aes(x=dates, label = "forecast\ndate", y = heights), size = 3, hjust = "right") +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

The fitted model here involved preprocessing the data to appropriately
generate lagged predictors, estimating a linear model with `stats::lm()`
and then postprocessing the results to be meaningful for epidemiological
tasks. We can also examine the predictions.

``` r
epiworkflow <- four_week_ahead$epi_workflow
restricted_predictions <-
  four_week_ahead$predictions |>
  filter(geo_value %in% plot_locations) |>
  rename(time_value = target_date, value = .pred) |>
  mutate(source = "death_rate")
forecast_plot <-
  narrow_data_plot |>
  epipredict:::plot_bands(
    restricted_predictions,
    levels = 0.9,
    fill = primary) +
  geom_point(data = restricted_predictions, aes(y = .data$value), color = secondary)
```

The results above show a distributional forecast produced using data
through the end of 2021 for the 14th of January 2022. A prediction for
the death rate per 100K inhabitants is available for every state
(`geo_value`) along with a 90% predictive interval.

<img src="man/figures/README-show-single-forecast-1.png" width="90%" style="display: block; margin: auto;" />
The yellow dot gives the median prediction, while the red interval gives
the 5-95% inter-quantile range. For this particular day and these
locations, the forecasts are relatively accurate, with the true data
being within the 25-75% interval. A couple of things to note:

1.  Our methods are primarily direct forecasters; this means we don’t
    need to predict 1, 2,…, 27 days ahead to then predict 28 days ahead
2.  All of our existing engines are geo-pooled, meaning the training
    data is shared across geographies. This has the advantage of
    increasing the amount of available training data, with the
    restriction that the data needs to be on comparable scales, such as
    rates.

## Getting Help

If you encounter a bug or have a feature request, feel free to file an
[issue on our github
page](https://github.com/cmu-delphi/epipredict/issues). For other
questions, feel free to contact [Daniel](daniel@stat.ubc.ca),
[David](davidweb@andrew.cmu.edu), [Dmitry](dshemetov@cmu.edu), or
[Logan](lcbrooks@andrew.cmu.edu), either via email or on the Insightnet
slack.

[^1]: This makes it so that any given day of the processed timeseries
    only depends on the previous week, which means that we avoid leaking
    future values when making a forecast.

[^2]: lagged by 3 in this context meaning using the value from 3 days
    ago.

[^3]: Alternatively, you could call `auto_plot(four_week_ahead)` to get
    the full collection of forecasts. This is too busy for the space we
    have for plotting here.
