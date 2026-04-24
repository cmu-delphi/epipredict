# warn when the latency is larger than would be reasonable

warn when the latency is larger than would be reasonable

## Usage

``` r
check_interminable_latency(
  dataset,
  latency_table,
  target_columns,
  forecast_date,
  call = caller_env()
)
```

## Arguments

- dataset:

  the epi_df

- latency_table:

  the whole collection of latencies

- target_columns:

  the names of the columns that we're adjusting, and whether its
  unreasonably latent
