# Format predictions for submission to FluSight forecast Hub

This function converts predictions from any of the included forecasters
into a format (nearly) ready for submission to the 2023-24
[FluSight-forecast-hub](https://github.com/cdcepi/FluSight-forecast-hub).
See there for documentation of the required columns. Currently, only
"quantile" forcasts are supported, but the intention is to support both
"quantile" and "pmf". For this reason, adding the `output_type` column
should be done via the `...` argument. See the examples below. The
specific required format for this forecast task is
[here](https://github.com/cdcepi/FluSight-forecast-hub/blob/main/model-output/README.md).

## Usage

``` r
flusight_hub_formatter(object, ..., .fcast_period = c("daily", "weekly"))
```

## Arguments

- object:

  a data.frame of predictions or an object of class `canned_epipred` as
  created by, e.g.,
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/dev/reference/arx_forecaster.md)

- ...:

  \<[`dynamic-dots`](https://rlang.r-lib.org/reference/dyn-dots.html)\>
  Name = value pairs of constant columns (or mutations) to perform to
  the results. See examples.

- .fcast_period:

  Control whether the `horizon` should represent days or weeks.
  Depending on whether the forecaster output has target dates from
  [`layer_add_target_date()`](https://cmu-delphi.github.io/epipredict/dev/reference/layer_add_target_date.md)
  or not, we may need to compute the horizon and/or the
  `target_end_date` from the other available columns in the predictions.
  When both `ahead` and `target_date` are available, this is ignored. If
  only `ahead` or `aheads` exists, then the target date may need to be
  multiplied if the `ahead` represents weekly forecasts. Alternatively,
  if only, the `target_date` is available, then the `horizon` will be in
  days, unless this argument is `"weekly"`. Note that these can be
  adjusted later by the `...` argument.

## Value

A [tibble::tibble](https://tibble.tidyverse.org/reference/tibble.html).
If `...` is empty, the result will contain the columns `reference_date`,
`horizon`, `target_end_date`, `location`, `output_type_id`, and `value`.
The `...` can perform mutations on any of these.

## Examples

``` r
library(dplyr)
library(epiprocess)
weekly_deaths <- covid_case_death_rates %>%
  filter(
    time_value >= as.Date("2021-09-01"),
    geo_value %in% c("ca", "ny", "dc", "ga", "vt")
  ) %>%
  select(geo_value, time_value, death_rate) %>%
  left_join(state_census %>% select(pop, abbr), by = c("geo_value" = "abbr")) %>%
  mutate(deaths = pmax(death_rate / 1e5 * pop * 7, 0)) %>%
  select(-pop, -death_rate) %>%
  group_by(geo_value) %>%
  epi_slide(~ sum(.$deaths), .window_size = 7, .new_col_name = "deaths_7dsum") %>%
  ungroup() %>%
  filter(weekdays(time_value) == "Saturday")

cdc <- cdc_baseline_forecaster(weekly_deaths, "deaths_7dsum")
flusight_hub_formatter(cdc)
#> # A tibble: 575 × 7
#>    reference_date horizon target_end_date location output_type_id value .pred
#>    <date>           <int> <date>          <chr>             <dbl> <dbl> <dbl>
#>  1 2021-12-25           1 2022-01-01      06                0.01  2147. 3166.
#>  2 2021-12-25           1 2022-01-01      06                0.025 2165. 3166.
#>  3 2021-12-25           1 2022-01-01      06                0.05  2215. 3166.
#>  4 2021-12-25           1 2022-01-01      06                0.1   2277. 3166.
#>  5 2021-12-25           1 2022-01-01      06                0.15  2594. 3166.
#>  6 2021-12-25           1 2022-01-01      06                0.2   2847. 3166.
#>  7 2021-12-25           1 2022-01-01      06                0.25  2886. 3166.
#>  8 2021-12-25           1 2022-01-01      06                0.3   2931. 3166.
#>  9 2021-12-25           1 2022-01-01      06                0.35  2977. 3166.
#> 10 2021-12-25           1 2022-01-01      06                0.4   3001. 3166.
#> # ℹ 565 more rows
flusight_hub_formatter(cdc, target = "wk inc covid deaths")
#> # A tibble: 575 × 8
#>    reference_date horizon target_end_date location output_type_id value .pred
#>    <date>           <int> <date>          <chr>             <dbl> <dbl> <dbl>
#>  1 2021-12-25           1 2022-01-01      06                0.01  2147. 3166.
#>  2 2021-12-25           1 2022-01-01      06                0.025 2165. 3166.
#>  3 2021-12-25           1 2022-01-01      06                0.05  2215. 3166.
#>  4 2021-12-25           1 2022-01-01      06                0.1   2277. 3166.
#>  5 2021-12-25           1 2022-01-01      06                0.15  2594. 3166.
#>  6 2021-12-25           1 2022-01-01      06                0.2   2847. 3166.
#>  7 2021-12-25           1 2022-01-01      06                0.25  2886. 3166.
#>  8 2021-12-25           1 2022-01-01      06                0.3   2931. 3166.
#>  9 2021-12-25           1 2022-01-01      06                0.35  2977. 3166.
#> 10 2021-12-25           1 2022-01-01      06                0.4   3001. 3166.
#> # ℹ 565 more rows
#> # ℹ 1 more variable: target <chr>
flusight_hub_formatter(cdc, target = paste(horizon, "wk inc covid deaths"))
#> # A tibble: 575 × 8
#>    reference_date horizon target_end_date location output_type_id value .pred
#>    <date>           <int> <date>          <chr>             <dbl> <dbl> <dbl>
#>  1 2021-12-25           1 2022-01-01      06                0.01  2147. 3166.
#>  2 2021-12-25           1 2022-01-01      06                0.025 2165. 3166.
#>  3 2021-12-25           1 2022-01-01      06                0.05  2215. 3166.
#>  4 2021-12-25           1 2022-01-01      06                0.1   2277. 3166.
#>  5 2021-12-25           1 2022-01-01      06                0.15  2594. 3166.
#>  6 2021-12-25           1 2022-01-01      06                0.2   2847. 3166.
#>  7 2021-12-25           1 2022-01-01      06                0.25  2886. 3166.
#>  8 2021-12-25           1 2022-01-01      06                0.3   2931. 3166.
#>  9 2021-12-25           1 2022-01-01      06                0.35  2977. 3166.
#> 10 2021-12-25           1 2022-01-01      06                0.4   3001. 3166.
#> # ℹ 565 more rows
#> # ℹ 1 more variable: target <chr>
flusight_hub_formatter(cdc, target = "wk inc covid deaths", output_type = "quantile")
#> # A tibble: 575 × 9
#>    reference_date horizon target_end_date location output_type_id value .pred
#>    <date>           <int> <date>          <chr>             <dbl> <dbl> <dbl>
#>  1 2021-12-25           1 2022-01-01      06                0.01  2147. 3166.
#>  2 2021-12-25           1 2022-01-01      06                0.025 2165. 3166.
#>  3 2021-12-25           1 2022-01-01      06                0.05  2215. 3166.
#>  4 2021-12-25           1 2022-01-01      06                0.1   2277. 3166.
#>  5 2021-12-25           1 2022-01-01      06                0.15  2594. 3166.
#>  6 2021-12-25           1 2022-01-01      06                0.2   2847. 3166.
#>  7 2021-12-25           1 2022-01-01      06                0.25  2886. 3166.
#>  8 2021-12-25           1 2022-01-01      06                0.3   2931. 3166.
#>  9 2021-12-25           1 2022-01-01      06                0.35  2977. 3166.
#> 10 2021-12-25           1 2022-01-01      06                0.4   3001. 3166.
#> # ℹ 565 more rows
#> # ℹ 2 more variables: target <chr>, output_type <chr>
```
