# checks: the recipe type, whether a previous step is the relevant epi_shift, that either `fixed_latency` or `fixed_forecast_date` is non-null, and that `fixed_latency` only references columns that exist at the time of the step inclusion

checks: the recipe type, whether a previous step is the relevant
epi_shift, that either `fixed_latency` or `fixed_forecast_date` is
non-null, and that `fixed_latency` only references columns that exist at
the time of the step inclusion

## Usage

``` r
step_adjust_latency_checks(
  id,
  method,
  recipe,
  fixed_latency,
  fixed_forecast_date,
  call = caller_env()
)
```
