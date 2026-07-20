# Calculate a climatological variable based on the history

`step_climate()` creates a *specification* of a recipe step that will
generate one or more new columns of derived data. This step examines all
available seasons in the training data and calculates the a measure of
center for the "typical" season. Think of this like with the weather: to
predict the temperature in January in Pittsburgh, PA, I might look at
all previous January's on record, average their temperatures, and
include that in my model. So it is important to *align* the forecast
horizon with the climate. This step will work best if added after
[`step_epi_ahead()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md),
but that is not strictly required. See the details for more information.

## Usage

``` r
step_climate(
  recipe,
  ...,
  forecast_ahead = "detect",
  role = "predictor",
  time_type = c("detect", "epiweek", "week", "month", "day"),
  center_method = c("median", "mean"),
  window_size = 3L,
  epi_keys = NULL,
  prefix = "climate_",
  skip = FALSE,
  id = rand_id("climate")
)
```

## Arguments

- recipe:

  A recipe object. The step will be added to the sequence of operations
  for this recipe.

- ...:

  One or more selector functions to choose variables for this step. See
  [`recipes::selections()`](https://recipes.tidymodels.org/reference/selections.html)
  for more details.

- forecast_ahead:

  The forecast horizon. By default, this step will try to detect whether
  a forecast horizon has already been specified with
  [`step_epi_ahead()`](https://cmu-delphi.github.io/epipredict/dev/reference/step_epi_shift.md).
  Alternatively, one can specify an explicit horizon with a scalar
  integer. Auto-detection is only possible when the time type of the
  `epi_df` used to create the `epi_recipe` is the same as the
  aggregation `time_type` specified in this step (say, both daily or
  both weekly). If, for example, daily data is used with monthly time
  aggregation, then auto-detection is not possible (and may in fact lead
  to strange behaviour even if `forecast_ahead` is specified with an
  integer). See details below.

- role:

  What role should be assigned for any variables created by this step?
  "predictor" is the most likely choice.

- time_type:

  The duration over which time aggregation should be performed.

- center_method:

  The measure of center to be calculated over the time window.

- window_size:

  Scalar integer. How many time units on each side should be included.
  For example, if `window_size = 3` and `time_type = "day"`, then on
  each day in the data, the center will be calculated using 3 days
  before and three days after. So, in this case, it operates like a
  weekly rolling average, centered at each day.

- epi_keys:

  Character vector or `NULL`. Any columns mentioned will be grouped
  before performing any center calculation. So for example, given
  state-level data, a national climate would be calculated if `NULL`,
  but passing `epi_keys = "geo_value"` would calculate the climate
  separately by state.

- prefix:

  A character string that will be prefixed to the new column.

- skip:

  A logical. Should the step be skipped when the recipe is baked by
  [`bake()`](https://recipes.tidymodels.org/reference/bake.html)? While
  all operations are baked when
  [`prep()`](https://recipes.tidymodels.org/reference/prep.html) is run,
  some operations may not be able to be conducted on new data (e.g.
  processing the outcome variable(s)). Care should be taken when using
  `skip = TRUE` as it may affect the computations for subsequent
  operations.

- id:

  A unique identifier for the step

## Value

An updated version of `recipe` with the new step added to the sequence
of any existing operations.

## Details

Construction of a climate predictor can be helpful with strongly
seasonal data. But its utility is greatest when the estimated "climate"
is aligned to the forecast horizon. For example, if today is December 1,
and we want to make a prediction for December 15, we want to know the
climate for the week of December 15 to use in our model. But we also
want to align the rest of our training data with the climate *2 weeks
after* those dates.

To accomplish this, if we have daily data, we could use
`time_type = "week"` and `forecast_ahead = 2`. The climate predictor
would be created by taking averages over each week (with a window of a
few weeks before and after, as determined by `window_size`), and then
aligning these with the appropriate dates in the training data so that
each `time_value` will "see" the typical climate 2 weeks in the future.

Alternatively, in the same scenario, we could use `time_type = "day"`
and `forecast_ahead = 14`. The climate predictor would be created by
taking averages over a small window around each *day*, and then aligning
these with the appropriate dates in the training data so that each
`time_value` will "see" the climate 14 days in the future.

The only differences between these options is the type of averaging
performed over the historical data. In the first case, days in the same
week will get the same value of the climate predictor (because we're
looking at weekly windows), while in the second case, every day in the
data will have the average climate for the *day* that happens 14 days in
the future.

Autodetecting the forecast horizon can only be guaranteed to work
correctly when the time types are the same: for example using daily data
for training and daily climate calculations. However, using weekly data,
predicting 4 weeks ahead, and setting `time_type = "month"` is perfectly
reasonable. It's just that the climate is calculated over *months*
(January, February, March, etc.) so how to properly align this when
producing a forecast for the 5th week in the year is challenging. For
scenarios like these, it may be best to approximately match the times
with `forecast_ahead = 1`, for example.

## Examples

``` r
# automatically detects the horizon
r <- epi_recipe(covid_case_death_rates) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_climate(death_rate, time_type = "day")
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
r
#> Error: object 'r' not found

r %>%
  prep(covid_case_death_rates) %>%
  bake(new_data = NULL)
#> Error: object 'r' not found

# same idea, but using weekly climate
r <- epi_recipe(covid_case_death_rates) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_climate(death_rate,
    forecast_ahead = 1, time_type = "epiweek",
    window_size = 1L
  )
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
r
#> Error: object 'r' not found

r %>%
  prep(covid_case_death_rates) %>%
  bake(new_data = NULL)
#> Error: object 'r' not found

# switching the order is possible if you specify `forecast_ahead`
r <- epi_recipe(covid_case_death_rates) %>%
  step_climate(death_rate, forecast_ahead = 7, time_type = "day") %>%
  step_epi_ahead(death_rate, ahead = 7)
#> Error in UseMethod("epi_recipe"): no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"
r
#> Error: object 'r' not found

r %>%
  prep(covid_case_death_rates) %>%
  bake(new_data = NULL)
#> Error: object 'r' not found
```
