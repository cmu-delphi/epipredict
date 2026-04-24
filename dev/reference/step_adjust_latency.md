# Adapt the model to latent data

In the standard case, the arx models assume that the last observation is
also the day from which the forecast is being made. But if the data has
latency, then you may wish to adjust the predictors (lags) and/or the
outcome (ahead) to compensate. This is most useful in realtime and
pseudo-prospective forecasting for data where there is some delay
between the event occurring and the event being reported.

## Usage

``` r
step_adjust_latency(
  recipe,
  ...,
  method = c("extend_ahead", "locf", "extend_lags"),
  epi_keys_checked = NULL,
  keys_to_ignore = c(),
  fixed_latency = NULL,
  fixed_forecast_date = NULL,
  check_latency_length = TRUE,
  id = rand_id("adjust_latency")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- method:

  a character. Determines the method by which the forecast handles
  latency. The options are:

  - `"extend_ahead"`: Lengthen the ahead so that forecasting from the
    last observation results in a forecast `ahead` after the
    `forecast_date` date. E.g. if there are 3 days of latency between
    the last observation and the `forecast_date` date for a 4 day ahead
    forecast, the ahead used in practice is actually 7.

  - `"locf"`: carries forward the last observed value(s) up to the
    forecast date.

  - `"extend_lags"`: per `epi_key` and `predictor`, adjusts the lag so
    that the shortest lag at predict time is at the last observation.
    E.g. if the lags are `c(0,7,14)` for data that is 3 days latent, the
    actual lags used become `c(3,10,17)`.

- epi_keys_checked:

  a character vector. A list of keys to group by before finding the
  `max_time_value` (the last day of data), defaulting to `geo_value`.
  Different locations may have different latencies; to produce a
  forecast at every location, we need to guarantee data at every
  location by using the largest latency across every location; this
  means taking `max_time_value` to be the minimum of the
  `max_time_value`s for each set of key values (so the earliest date).
  If `NULL` or an empty character vector, it will take the maximum
  across all values, irrespective of any keys.

  Note that this is a separate concern from different latencies across
  different *data columns*, which is only handled by the choice of
  `method`.

- keys_to_ignore:

  a list of character vectors. Set this to avoid using specific key
  values in the `epi_keys_checked` to set latency. For example, say you
  have two locations `pr` and `gu` which have useful training data, but
  have stopped providing up-to-date information, and so are no longer
  part of the test set. Setting
  `keys_to_ignore = list(geo_value = c("pr", "gu"))` will exclude them
  from the latency calculation.

- fixed_latency:

  either a positive integer, or a labeled positive integer vector.
  Cannot be set at the same time as `fixed_forecast_date`. If
  non-`NULL`, the amount to offset the ahead or lag by. If a single
  integer, this is used for all columns; if a labeled vector, the labels
  must correspond to the base column names (before lags/aheads). If
  `NULL`, the latency is the distance between the `epi_df`'s
  `max_time_value` and the `forecast_date`.

- fixed_forecast_date:

  either a date of the same kind used in the `epi_df`, or `NULL`.
  Exclusive with `fixed_latency`. If a date, it gives the date from
  which the forecast is actually occurring. If `NULL`, the
  `forecast_date` is determined either via the `fixed_latency`, or is
  set to the `epi_df`'s `as_of` value if `fixed_latency` is also `NULL`.

- check_latency_length:

  bool, determines whether to warn if the latency is unusually high.
  Turn off if you know your forecast is going to be far into the future.

- id:

  A character string that is unique to this step to identify it.

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

This step allows the user to create models on the most recent data,
automatically accounting for latency patterns. Instead of using the last
observation date, `step_adjust_latency` uses the `as_of` date of the
`epi_df` as the `forecast_date`, and adjusts the model so that there is
data available. To demonstrate some of the subtleties, let's consider a
toy dataset:

    toy_df <- tribble(
     ~geo_value, ~time_value, ~a, ~b,
     "ma", as.Date("2015-01-11"), 20, 6,
     "ma", as.Date("2015-01-12"), 23, NA,
     "ma", as.Date("2015-01-13"), 25, NA,
     "ca", as.Date("2015-01-11"), 100, 5,
     "ca", as.Date("2015-01-12"), 103, 10,
    ) %>%
       as_epi_df(as_of = as.Date("2015-01-14"))

If we're looking to predict the value on the 15th, forecasting from the
14th (the `as_of` date above), there are two issues we will need to
address:

1.  `"ca"` is latent by 2 days, whereas `"ma"` is latent by 1

2.  if we want to use `b` as an exogenous variable, for `"ma"` it is
    latent by 3 days instead of just 1.

Regardless of `method`, `epi_keys_checked="geo_value"` guarantees tha
the difference between `"ma"` and `"ca"` is accounted for by making the
latency adjustment at least 2. For some comparison, here's what the
various methods will do:

### `locf`

Short for "last observation carried forward", `locf` assumes that every
day between the last observation and the forecast day is exactly the
same. This is a very straightforward assumption, but wrecks any features
that depend on changes in value over time, such as the growth rate, or
even adjacent lags. A more robust version of this falls under the
heading of nowcasting, an eventual aim for this package. On the toy
dataset, it doesn't matter which day we're trying to predict, since it
just fills forward to the `forecast_date`:

    toy_recipe <- epi_recipe(toy_df) %>%
      step_adjust_latency(has_role("raw"), method="locf")

    toy_recipe %>%
      prep(toy_df) %>%
      bake(toy_df) %>%
      arrange(geo_value, time_value)
    #> An `epi_df` object, 8 x 4 with metadata:
    #> * geo_type  = state
    #> * time_type = day
    #> * as_of     = 2015-01-14
    #> Latency (lag between last available observation and epi_df's as_of, by time series):
    #> * lag across all time series = 0 days
    #>
    #> # A tibble: 8 x 4
    #>   geo_value time_value     a     b
    #>   <chr>     <date>     <dbl> <dbl>
    #> 1 ca        2015-01-11   100     5
    #> 2 ca        2015-01-12   103    10
    #> 3 ca        2015-01-13   103    10
    #> 4 ca        2015-01-14   103    10
    #> 5 ma        2015-01-11    20     6
    #> 6 ma        2015-01-12    23     6
    #> 7 ma        2015-01-13    25     6
    #> 8 ma        2015-01-14    25     6

### `extend_lags`

`extend_lags` increases the lags so that they are guaranteed to have
data. This has the advantage of being applicable on a per-column basis;
if cases and deaths are reported at different latencies, the lags for
each are adjusted separately. In the toy example:

    toy_recipe <- epi_recipe(toy_df) %>%
      step_adjust_latency(has_role("raw"), method = "extend_lags") %>%
      step_epi_lag(a, lag=1) %>%
      step_epi_lag(b, lag=1) %>%
      step_epi_ahead(a, ahead=1)

    toy_recipe %>%
      prep(toy_df) %>%
      bake(toy_df) %>%
      arrange(geo_value, time_value)
    #> An `epi_df` object, 21 x 7 with metadata:
    #> * geo_type  = state
    #> * time_type = day
    #> * as_of     = 2015-01-14
    #> Latency (lag between last available observation and epi_df's as_of, by time series):
    #> * lag  = -2–3 days
    #>
    #> # A tibble: 21 x 7
    #>    geo_value time_value     a     b lag_3_a lag_4_b ahead_1_a
    #>    <chr>     <date>     <dbl> <dbl>   <dbl>   <dbl>     <dbl>
    #>  1 ca        2015-01-10    NA    NA      NA      NA       100
    #>  2 ca        2015-01-11   100     5      NA      NA       103
    #>  3 ca        2015-01-12   103    10      NA      NA        NA
    #>  4 ca        2015-01-13    NA    NA      NA      NA        NA
    #>  5 ca        2015-01-14    NA    NA     100      NA        NA
    #>  6 ca        2015-01-15    NA    NA     103       5        NA
    #>  7 ca        2015-01-16    NA    NA      NA      10        NA
    #>  8 ca        2015-01-17    NA    NA      NA      NA        NA
    #>  9 ca        2015-01-18    NA    NA      NA      NA        NA
    #> 10 ca        2015-01-19    NA    NA      NA      NA        NA
    #> # i 11 more rows

The maximum latency in column `a` is 2 days, so the lag is increased to
3, while the max latency in column `b` is 3, so the same lag is
increased to 4; both of these changes are reflected in the column names.
Meanwhile the ahead is uneffected.

As a side-note, lag/ahead can be somewhat ambiguous about direction.
Here, the values are brought forward in time, so that for a given row,
column `lag_3_a` represents the value 3 days before.

### `extend_ahead`

`extend_ahead` increases the ahead, turning a 3 day ahead forecast into
a 7 day one; this has the advantage of simplicity and is reflective of
the actual modelling task, but potentially leaves information unused if
different data sources have different latencies; it must use the latency
of the most latent data to insure there is data available. In the toy
example:

    toy_recipe <- epi_recipe(toy_df) %>%
      step_adjust_latency(has_role("raw"), method="extend_ahead") %>%
      step_epi_lag(a, lag=0) %>%
      step_epi_ahead(a, ahead=1)

    toy_recipe %>%
      prep(toy_df) %>%
      bake(toy_df) %>%
      arrange(geo_value, time_value)
    #> An `epi_df` object, 10 x 6 with metadata:
    #> * geo_type  = state
    #> * time_type = day
    #> * as_of     = 2015-01-14
    #> Latency (lag between last available observation and epi_df's as_of, by time series):
    #> * lag  = 1–5 days
    #>
    #> # A tibble: 10 x 6
    #>    geo_value time_value     a     b lag_0_a ahead_3_a
    #>    <chr>     <date>     <dbl> <dbl>   <dbl>     <dbl>
    #>  1 ca        2015-01-08    NA    NA      NA       100
    #>  2 ca        2015-01-09    NA    NA      NA       103
    #>  3 ca        2015-01-11   100     5     100        NA
    #>  4 ca        2015-01-12   103    10     103        NA
    #>  5 ma        2015-01-08    NA    NA      NA        20
    #>  6 ma        2015-01-09    NA    NA      NA        23
    #>  7 ma        2015-01-10    NA    NA      NA        25
    #>  8 ma        2015-01-11    20     6      20        NA
    #>  9 ma        2015-01-12    23    NA      23        NA
    #> 10 ma        2015-01-13    25    NA      25        NA

Even though we're doing a 1 day ahead forecast, because our worst
latency is 3 days from column `b`'s `"ma"` data, our outcome column is
`ahead_4_a` (so 4 days ahead). If we want to ignore any latency in
column `b`, we need to explicitly set the columns to consider while
adjusting like this: `step_adjust_latency(a, method="extend_ahead")`.

## Programmatic details

`step_adjust_latency` uses the metadata, such as `time_type` and
`as_of`, of the `epi_df` used in the initial prep step, rather than
baking or prediction. This means reusing the same forecaster on new data
is not advised, though typically it is not advised in general.

The latency adjustment only applies to columns created after this step,
so this step should go before both `step_epi_ahead` and `step_epi_lag`.
This will work:

    toy_recipe <- epi_recipe(toy_df) %>%
       # non-lag steps
       step_adjust_latency(a, method = "extend_lags") %>%
       step_epi_lag(a, lag=0) # other steps

while this will not:

    toy_recipe <- epi_recipe(toy_df) %>%
       step_epi_lag(a, lag=0) %>%
       step_adjust_latency(a, method = "extend_lags")
    #> Warning: If `method` is "extend_lags" or "locf", then the previous `step_epi_lag`s won't
    #> work with modified data.

If you create columns that you then apply lags to (such as
[`step_growth_rate()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_growth_rate.md)),
these should be created before `step_adjust_latency`, so any subseqent
latency can be addressed.

## See also

Other row operation steps:
[`step_epi_lag()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md),
[`step_growth_rate()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_growth_rate.md),
[`step_lag_difference()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_lag_difference.md)

## Examples

``` r
rates <- covid_case_death_rates %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
# setting the `as_of` to something realistic
attributes(rates)$metadata$as_of <- max(rates$time_value) + 3

r <- epi_recipe(rates) %>%
  step_adjust_latency(recipes::has_role("raw"), method = "extend_ahead") %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14))
r
#> 
#> ── Epi Recipe ──────────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> raw:        2
#> geo_value:  1
#> time_value: 1
#> 
#> ── Operations 
#> 1. Adj. extend_ahead: recipes::has_role("raw") latency TBD at train time
#> 2. Leading: death_rate by 7
#> 3. Lagging: death_rate by 0, 7, 14

rates_fit <- epi_workflow() %>%
  add_epi_recipe(r) %>%
  add_model(linear_reg()) %>%
  fit(data = rates)
rates_fit
#> 
#> ══ Epi Workflow [trained] ══════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> Postprocessor: None
#> 
#> ── Preprocessor ────────────────────────────────────────────────────────────────
#> 
#> 3 Recipe steps.
#> 1. step_adjust_latency()
#> 2. step_epi_ahead()
#> 3. step_epi_lag()
#> 
#> ── Model ───────────────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#>       (Intercept)   lag_0_death_rate   lag_7_death_rate  lag_14_death_rate  
#>            0.3806            -0.2208            -0.0403            -0.0394  
#> 
#> 
```
