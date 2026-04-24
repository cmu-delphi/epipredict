# Shift predictors while maintaining grouping and time_value ordering

This is a lower-level function. As such it performs no error checking.

## Usage

``` r
epi_shift_single(x, col, shift_val, newname, key_cols)
```

## Arguments

- x:

  Data frame.

- shift_val:

  a single integer. Negative values produce leads.

- newname:

  the name for the newly shifted column

- key_cols:

  vector, or `NULL`. Additional grouping vars.

## Value

a list of tibbles
