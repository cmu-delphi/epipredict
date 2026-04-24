# create the latency table This is a table of column names and the latency adjustment necessary for that column. An example:

col_name latency 1 case_rate 5 2 death_rate 5

## Usage

``` r
get_latency_table(
  training,
  columns,
  forecast_date,
  latency,
  sign_shift,
  epi_keys_checked,
  keys_to_ignore,
  info,
  terms
)
```
