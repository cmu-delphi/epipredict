# the latency is also the amount the shift is off by

the latency is also the amount the shift is off by

## Usage

``` r
get_latency(new_data, forecast_date, column, sign_shift, epi_keys_checked)
```

## Arguments

- sign_shift:

  integer. 1 if lag and -1 if ahead. These represent how you need to
  shift the data to bring the 3 day lagged value to today.
