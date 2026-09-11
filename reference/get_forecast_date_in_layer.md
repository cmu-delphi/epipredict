# get the target date while in a layer

get the target date while in a layer

## Usage

``` r
get_forecast_date_in_layer(this_recipe, workflow_max_time_value, new_data)
```

## Arguments

- this_recipe:

  the recipe to check for `step_adjust_latency`

- workflow_max_time_value:

  the `max_time` value coming out of the fit workflow (this will be the
  maximal time value in a potentially different dataset)

- new_data:

  the data we're currently working with, from which we'll take a
  potentially different max_time_value
