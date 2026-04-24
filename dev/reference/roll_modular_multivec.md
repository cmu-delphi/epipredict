# group col by .idx values and sum windows around each .idx value

group col by .idx values and sum windows around each .idx value

## Usage

``` r
roll_modular_multivec(col, idx_in, weights, aggr, window_size, modulus)
```

## Arguments

- col:

  the list of values indexed by `idx_in`

- idx_in:

  the relevant periodic part of time value, e.g. the week number,
  limited to the relevant range

- weights:

  how much to weigh each particular datapoint (also indexed by `idx_in`)

- aggr:

  the aggregation function, probably Quantile, mean, or median

- window_size:

  the number of .idx entries before and after to include in the
  aggregation

- modulus:

  the number of days/weeks/months in the year, not including any leap
  days/weeks
