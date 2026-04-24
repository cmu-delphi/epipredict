# Predict the future with today's value

This is a simple forecasting model for
[epiprocess::epi_df](https://cmu-delphi.github.io/epiprocess/reference/epi_df.html)
data. It uses the most recent observation as the forecast for any future
date, and produces intervals based on the quantiles of the residuals of
such a "flatline" forecast over all available training data.

By default, the predictive intervals are computed separately for each
combination of key values (`geo_value` + any additional keys) in the
`epi_data` argument.

This forecaster is very similar to that used by the
[COVID19ForecastHub](https://covid19forecasthub.org)

## Usage

``` r
flatline_forecaster(epi_data, outcome, args_list = flatline_args_list())
```

## Arguments

- epi_data:

  An
  [epiprocess::epi_df](https://cmu-delphi.github.io/epiprocess/reference/epi_df.html)

- outcome:

  A scalar character for the column name we wish to predict.

- args_list:

  A list of additional arguments as created by the
  [`flatline_args_list()`](https://cmu-delphi.github.io/epipredict/dev/reference/flatline_args_list.md)
  constructor function.

## Value

A data frame of point (and optionally interval) forecasts at a single
ahead (unique horizon) for each unique combination of `key_vars`.

## Details

Here is (roughly) the code for the `flatline_forecaster()` applied to
the `case_rate` for
[`epidatasets::covid_case_death_rates`](https://cmu-delphi.github.io/epidatasets/reference/covid_case_death_rates.html).

    jhu <- covid_case_death_rates %>%
      filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
    r <- epi_recipe(covid_case_death_rates) %>%
      step_epi_ahead(case_rate, ahead = 7, skip = TRUE) %>%
      recipes::update_role(case_rate, new_role = "predictor") %>%
      recipes::add_role(all_of(key_colnames(jhu)), new_role = "predictor")

    f <- frosting() %>%
      layer_predict() %>%
      layer_residual_quantiles() %>%
      layer_add_forecast_date() %>%
      layer_add_target_date() %>%
      layer_threshold(starts_with(".pred"))

    eng <- linear_reg() %>% set_engine("flatline")
    wf <- epi_workflow(r, eng, f) %>% fit(jhu)
    preds <- forecast(wf)

## Examples

``` r
jhu <- covid_case_death_rates %>%
  dplyr::filter(time_value >= as.Date("2021-12-01"))

out <- flatline_forecaster(jhu, "death_rate")
```
