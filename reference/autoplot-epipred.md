# Automatically plot an `epi_workflow` or `canned_epipred` object

For a fit workflow, the training data will be displayed, the response by
default. If `predictions` is not `NULL` then point and interval
forecasts will be shown as well. Unfit workflows will result in an
error, (you can simply call
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html) on
the original `epi_df`).

## Usage

``` r
# S3 method for class 'epi_workflow'
autoplot(
  object,
  predictions = NULL,
  observed_response = NULL,
  .levels = c(0.5, 0.8, 0.9),
  ...,
  .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
  .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
  .base_color = "dodgerblue4",
  .point_pred_color = "orange",
  .facet_filter = NULL
)

# S3 method for class 'canned_epipred'
autoplot(
  object,
  observed_response = NULL,
  ...,
  .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
  .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
  .base_color = "dodgerblue4",
  .point_pred_color = "orange",
  .facet_filter = NULL
)

# S3 method for class 'epi_workflow'
plot(x, ...)

# S3 method for class 'canned_epipred'
plot(x, ...)
```

## Arguments

- object, x:

  An `epi_workflow`

- predictions:

  A data frame with predictions. If `NULL`, only the original data is
  shown.

- observed_response:

  An epi_df of the data to plot against. This is for the case where you
  have the actual results to compare the forecast against.

- .levels:

  A numeric vector of levels to plot for any prediction bands. More than
  3 levels begins to be difficult to see.

- ...:

  Ignored

- .color_by:

  Which variables should determine the color(s) used to plot lines.
  Options include:

  - `all_keys` - the default uses the interaction of any key variables
    including the `geo_value`

  - `geo_value` - `geo_value` only

  - `other_keys` - any available keys that are not `geo_value`

  - `.response` - the numeric variables (same as the y-axis)

  - `all` - uses the interaction of all keys and numeric variables

  - `none` - no coloring aesthetic is applied

- .facet_by:

  Similar to `.color_by` except that the default is to display the
  response.

- .base_color:

  If available, prediction bands will be shown with this color.

- .point_pred_color:

  If available, point forecasts will be shown with this color.

- .facet_filter:

  Select which facets will be displayed. Especially useful for when
  there are many `geo_value`'s or keys. This is a
  \<[`rlang`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  expression along the lines of
  [`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).
  However, it must be a single expression combined with the `&`
  operator. This contrasts to the typical use case which allows multiple
  comma-separated expressions which are implicitly combined with `&`.
  When multiple variables are selected with `...`, their names can be
  filtered in combination with other factors by using `.response_name`.
  See the examples below.

## Examples

``` r
jhu <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-11-01"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_epi_naomit()

f <- frosting() %>%
  layer_residual_quantiles() %>%
  layer_threshold(starts_with(".pred")) %>%
  layer_add_target_date()

wf <- epi_workflow(r, linear_reg(), f) %>% fit(jhu)

autoplot(wf)


latest <- jhu %>% filter(time_value >= max(time_value) - 14)
preds <- predict(wf, latest)
autoplot(wf, preds, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt"))


# ------- Show multiple horizons

p <- lapply(c(7, 14, 21, 28), function(h) {
  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = h) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_epi_naomit()
  ewf <- epi_workflow(r, linear_reg(), f) %>% fit(jhu)
  forecast(ewf)
})

p <- vctrs::vec_rbind(!!!p)
autoplot(wf, p, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt"))


# ------- Plotting canned forecaster output

jhu <- covid_case_death_rates %>%
  filter(time_value >= as.Date("2021-11-01"))
flat <- flatline_forecaster(jhu, "death_rate")
autoplot(flat, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt"))
#> Warning: Removed 28 rows containing missing values or values outside the scale range
#> (`geom_line()`).


arx <- arx_forecaster(jhu, "death_rate", c("case_rate", "death_rate"),
  args_list = arx_args_list(ahead = 14L)
)
autoplot(arx, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt", "mo", "in"))
```
