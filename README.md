
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Epipredict

<!-- badges: start -->

[![R-CMD-check](https://github.com/cmu-delphi/epipredict/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmu-delphi/epipredict/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

[`{epipredict}`](https://cmu-delphi.github.io/epipredict/) is a
framework for building transformation and forecasting pipelines for
epidemiological and other panel time-series datasets. In addition to
tools for building forecasting pipelines, it contains a number of
“canned” forecasters meant to run with little modification as an easy
way to get started forecasting.

It is designed to work well with
[`{epiprocess}`](https://cmu-delphi.github.io/epiprocess/), a utility
for time series handling and geographic processing in an epidemiological
context. Both of the packages are meant to work well with the panel data
provided by [`{epidatr}`](https://cmu-delphi.github.io/epidatr/).
Pre-compiled example datasets are also available in
[`{epidatasets}`](https://cmu-delphi.github.io/epidatasets/).

If you are looking for detail beyond the package documentation, see our
[forecasting book](https://cmu-delphi.github.io/delphi-tooling-book/).

## Installation

Unless you’re planning on contributing to package development, we
suggest using the stable version. To install, run:

``` r
# Stable version
pak::pkg_install("cmu-delphi/epipredict@main")

# Dev version
pak::pkg_install("cmu-delphi/epipredict@dev")
```

The documentation for the stable version is at
<https://cmu-delphi.github.io/epipredict>, while the development version
is at <https://cmu-delphi.github.io/epipredict/dev>.

## Motivating example

<details>
<summary>
Required packages
</summary>

``` r
library(epipredict)
library(epidatr)
library(epiprocess)
library(dplyr)
library(ggplot2)
```

</details>

To demonstrate using
[`{epipredict}`](https://cmu-delphi.github.io/epipredict/) for
forecasting, say we want to predict COVID-19 deaths per 100k people for
each of a subset of states

``` r
used_locations <- c("ca", "ma", "ny", "tx")
```

on

``` r
forecast_date <- as.Date("2021-08-01")
```

We will be using a subset of [Johns Hopkins Center for Systems Science
and Engineering deaths
data](https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html).
Below the fold, we pull the dataset from the epidata API and clean it.

<details>
<summary>
Creating the dataset using `{epidatr}` and `{epiprocess}`
</summary>

This section is intended to demonstrate some of the ubiquitous cleaning
operations needed to be able to forecast. A subset of this dataset
prepared here is also included ready-to-go in
[`{epipredict}`](https://cmu-delphi.github.io/epipredict/) as
`covid_case_death_rates`.

First we pull both `jhu-csse` cases and deaths data from the [Delphi
API](https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html)
using the [`{epidatr}`](https://cmu-delphi.github.io/epidatr/) package:

``` r
cases <- pub_covidcast(
  source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20211231),
  geo_values = "*"
) |>
  select(geo_value, time_value, case_rate = value)

deaths <- pub_covidcast(
  source = "jhu-csse",
  signals = "deaths_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20211231),
  geo_values = "*"
) |>
  select(geo_value, time_value, death_rate = value)
cases_deaths <-
  full_join(cases, deaths, by = c("time_value", "geo_value")) |>
  filter(geo_value %in% used_locations) |>
  as_epi_df(as_of = as.Date("2022-01-01"))
```

Since visualizing the results on every geography is somewhat
overwhelming, we’ll only train on a subset of locations.

``` r
# plotting the data as it was downloaded
cases_deaths |>
  autoplot(
    case_rate,
    death_rate,
    .color_by = "none"
  ) +
  facet_grid(
    rows = vars(.response_name),
    cols = vars(geo_value),
    scale = "free"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

<img src="man/figures/README-date-1.png" width="90%" style="display: block; margin: auto;" />

As with the typical dataset, we will need to do some cleaning to make it
actually usable; we’ll use some utilities from
[`{epiprocess}`](https://cmu-delphi.github.io/epiprocess/) for this.
Specifically we’ll trim outliers, especially negative values:

``` r
cases_deaths <-
  cases_deaths |>
  group_by(geo_value) |>
  mutate(
    outlr_death_rate = detect_outlr_rm(
      time_value, death_rate,
      detect_negatives = TRUE
    ),
    outlr_case_rate = detect_outlr_rm(
      time_value, case_rate,
      detect_negatives = TRUE
    )
  ) |>
  unnest(cols = starts_with("outlr"), names_sep = "_") |>
  ungroup() |>
  mutate(
    death_rate = outlr_death_rate_replacement,
    case_rate = outlr_case_rate_replacement
  ) |>
  select(geo_value, time_value, case_rate, death_rate)
```

</details>

After downloading and cleaning the cases and deaths data, we can plot a
subset of the states, marking the desired forecast date:

<details>
<summary>
Plot
</summary>

``` r
forecast_date_label <-
  tibble(
    geo_value = rep(used_locations, 2),
    .response_name = c(rep("case_rate", 4), rep("death_rate", 4)),
    dates = rep(forecast_date - 7 * 2, 2 * length(used_locations)),
    heights = c(rep(150, 4), rep(0.75, 4))
  )
processed_data_plot <-
  cases_deaths |>
  filter(geo_value %in% used_locations) |>
  autoplot(
    case_rate,
    death_rate,
    .color_by = "none"
  ) +
  facet_grid(
    rows = vars(.response_name),
    cols = vars(geo_value),
    scale = "free"
  ) +
  geom_vline(aes(xintercept = forecast_date)) +
  geom_text(
    data = forecast_date_label,
    aes(x = dates, label = "forecast\ndate", y = heights),
    size = 3, hjust = "right"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

</details>

<img src="man/figures/README-show-processed-data-1.png" width="90%" style="display: block; margin: auto;" />

To make a forecast, we will use a simple “canned” auto-regressive
forecaster to predict the death rate four weeks into the future using
lagged[^1] deaths and cases.

``` r
four_week_ahead <- arx_forecaster(
  cases_deaths |> filter(time_value <= forecast_date),
  outcome = "death_rate",
  predictors = c("case_rate", "death_rate"),
  args_list = arx_args_list(
    lags = list(c(0, 1, 2, 3, 7, 14), c(0, 7, 14)),
    ahead = 4 * 7,
    quantile_levels = c(0.1, 0.25, 0.5, 0.75, 0.9)
  )
)
four_week_ahead
#> ══ A basic forecaster of type ARX Forecaster ════════════════════════════════
#> 
#> This forecaster was fit on 2025-03-03 14:43:07.
#> 
#> Training data was an <epi_df> with:
#> • Geography: state,
#> • Time type: day,
#> • Using data up-to-date as of: 2022-01-01.
#> • With the last data available on 2021-08-01
#> 
#> ── Predictions ──────────────────────────────────────────────────────────────
#> 
#> A total of 4 predictions are available for
#> • 4 unique geographic regions,
#> • At forecast date: 2021-08-01,
#> • For target date: 2021-08-29,
#> 
```

In our model setup, we are defining as our predictors case rate lagged
0-3 days, one week, and two weeks, and death rate lagged 0-2 weeks. The
result `four_week_ahead` is both a fitted model object which could be
used any time in the future to create different forecasts, and a set of
predicted values (and prediction intervals) for each location 28 days
after the forecast date.

Plotting the prediction intervals on the true values for our location
subset[^2]:

<details>
<summary>
Plot
</summary>

``` r
epiworkflow <- four_week_ahead$epi_workflow
restricted_predictions <-
  four_week_ahead$predictions |>
  rename(time_value = target_date, value = .pred) |>
  mutate(.response_name = "death_rate")
forecast_plot <-
  four_week_ahead |>
  autoplot(plot_data = cases_deaths) +
  geom_vline(aes(xintercept = forecast_date)) +
  geom_text(
    data = forecast_date_label %>% filter(.response_name == "death_rate"),
    aes(x = dates, label = "forecast\ndate", y = heights),
    size = 3, hjust = "right"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

</details>

<img src="man/figures/README-show-single-forecast-1.png" width="90%" style="display: block; margin: auto;" />

And as a tibble of quantile level-value pairs:

``` r
four_week_ahead$predictions |>
  select(-.pred) |>
  pivot_quantiles_longer(.pred_distn)
#> # A tibble: 20 × 5
#>   geo_value values quantile_levels forecast_date target_date
#>   <chr>      <dbl>           <dbl> <date>        <date>     
#> 1 ca        0.0425            0.1  2021-08-01    2021-08-29 
#> 2 ca        0.0803            0.25 2021-08-01    2021-08-29 
#> 3 ca        0.115             0.5  2021-08-01    2021-08-29 
#> 4 ca        0.150             0.75 2021-08-01    2021-08-29 
#> 5 ca        0.187             0.9  2021-08-01    2021-08-29 
#> 6 ma        0                 0.1  2021-08-01    2021-08-29 
#> # ℹ 14 more rows
```

The yellow dot gives the median prediction, while the blue intervals
give the 25-75%, the 10-90%, and 2.5-97.5% inter-quantile ranges[^3].
For this particular day and these locations, the forecasts are
relatively accurate, with the true data being at least within the 10-90%
interval. A couple of things to note:

1.  Our methods are primarily direct forecasters; this means we don’t
    need to predict 1, 2,…, 27 days ahead to then predict 28 days ahead
2.  All of our existing engines are geo-pooled, meaning the training
    data is shared across geographies. This has the advantage of
    increasing the amount of available training data, with the
    restriction that the data needs to be on comparable scales, such as
    rates.

## Getting Help

If you encounter a bug or have a feature request, feel free to file an
[issue on our GitHub
page](https://github.com/cmu-delphi/epipredict/issues). For other
questions, feel free to reach out to the authors, either via this
[contact
form](https://docs.google.com/forms/d/e/1FAIpQLScqgT1fKZr5VWBfsaSp-DNaN03aV6EoZU4YljIzHJ1Wl_zmtg/viewform),
email, or the InsightNet Slack.

[^1]: lagged by 3 in this context meaning using the value from 3 days
    ago.

[^2]: Alternatively, you could call
    `autoplot(four_week_ahead, plot_data = cases_deaths)` to get the
    full collection of forecasts. This is too busy for the space we have
    for plotting here.

[^3]: Note that these are not the same quantiles that we fit when
    creating `four_week_ahead`. They are extrapolated from those
    quantiles using `extrapolate_quantiles()` (which assumes an
    exponential decay in the tails).
