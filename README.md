
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Epipredict

<!-- badges: start -->

[![R-CMD-check](https://github.com/cmu-delphi/epipredict/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cmu-delphi/epipredict/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Epipredict is a framework for building transformation and forecasting
pipelines for epidemiological and other panel time-series datasets. In
addition to tools for building forecasting pipelines, it contains a
number of “canned” forecasters meant to run with little modification as
an easy way to get started forecasting.

It is designed to work well with
[`epiprocess`](https://cmu-delphi.github.io/epiprocess/), a utility for
handling various time series and geographic processing tools in an
epidemiological context. Both of the packages are meant to work well
with the panel data provided by
[`epidatr`](https://cmu-delphi.github.io/epidatr/).

If you are looking for more detail beyond the package documentation, see
our [forecasting
book](https://cmu-delphi.github.io/delphi-tooling-book/).

## Installation

To install (unless you’re planning on contributing to package
development, we suggest using the stable version):

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

To demonstrate the kind of forecast epipredict can make, say we’re
predicting COVID deaths per 100k for each state on

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
  time_values = epirange(20200601, 20211231),
  geo_values = "*"
) |>
  select(geo_value, time_value, case_rate = value)

deaths <- pub_covidcast(
  source = "jhu-csse",
  signals = "deaths_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200601, 20211231),
  geo_values = "*"
) |>
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
    outlr_death_rate = detect_outlr_rm(
      time_value, death_rate, detect_negatives = TRUE
    ),
    outlr_case_rate = detect_outlr_rm(
      time_value, case_rate, detect_negatives = TRUE
    )
  ) |>
  unnest(cols = starts_with("outlr"), names_sep = "_") |>
  ungroup() |>
  mutate(
    death_rate = outlr_death_rate_replacement,
    case_rate = outlr_case_rate_replacement
  ) |>
  select(geo_value, time_value, case_rate, death_rate)
cases_deaths
#> An `epi_df` object, 32,424 x 4 with metadata:
#> * geo_type  = state
#> * time_type = day
#> * as_of     = 2022-01-01
#> 
#> # A tibble: 32,424 × 4
#>   geo_value time_value case_rate death_rate
#> * <chr>     <date>         <dbl>      <dbl>
#> 1 ak        2020-06-01      2.31          0
#> 2 ak        2020-06-02      1.94          0
#> 3 ak        2020-06-03      2.63          0
#> 4 ak        2020-06-04      2.59          0
#> 5 ak        2020-06-05      2.43          0
#> 6 ak        2020-06-06      2.35          0
#> # ℹ 32,418 more rows
```

</details>

After having downloaded and cleaned the data in `cases_deaths`, we plot
a subset of the states, noting the actual forecast date:

<details>
<summary>
Plot
</summary>

``` r
forecast_date_label <-
  tibble(
    geo_value = rep(plot_locations, 2),
    source = c(rep("case_rate", 4), rep("death_rate", 4)),
    dates = rep(forecast_date - 7 * 2, 2 * length(plot_locations)),
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
    data = forecast_date_label,
    aes(x = dates, label = "forecast\ndate", y = heights),
    size = 3, hjust = "right"
  ) +
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
    ahead = 4 * 7
  )
)
four_week_ahead
#> ══ A basic forecaster of type ARX Forecaster ════════════════════════════════
#> 
#> This forecaster was fit on 2025-01-24 14:47:38.
#> 
#> Training data was an <epi_df> with:
#> • Geography: state,
#> • Time type: day,
#> • Using data up-to-date as of: 2022-01-01.
#> • With the last data available on 2021-08-01
#> 
#> ── Predictions ──────────────────────────────────────────────────────────────
#> 
#> A total of 56 predictions are available for
#> • 56 unique geographic regions,
#> • At forecast date: 2021-08-01,
#> • For target date: 2021-08-29,
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
    data = forecast_date_label,
    aes(x = dates, label = "forecast\ndate", y = heights),
    size = 3, hjust = "right"
  ) +
  scale_x_date(date_breaks = "3 months", date_labels = "%Y %b") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

Putting that together with a plot of the bands, and a plot of the median
prediction.

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
    fill = primary
  ) +
  geom_point(data = restricted_predictions,
             aes(y = .data$value),
             color = secondary)
```

</details>

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
