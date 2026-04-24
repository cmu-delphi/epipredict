# generate the idx values within `window_size` of `target_idx` given that our time value is of the type matching modulus

generate the idx values within `window_size` of `target_idx` given that
our time value is of the type matching modulus

## Usage

``` r
within_window(target_idx, window_size, modulus)
```

## Arguments

- target_idx:

  the time index which we're drawing the window around

- window_size:

  the size of the window on one side of `target_idx`

- modulus:

  the number of days/weeks/months in the year, not including any leap
  days/weeks
