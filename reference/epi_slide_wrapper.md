# Wrapper to handle epi_slide particulars

This should simplify somewhat in the future when we can run `epi_slide`
on columns. Surprisingly, lapply is several orders of magnitude faster
than using roughly equivalent tidy select style.

## Usage

``` r
epi_slide_wrapper(
  new_data,
  .window_size,
  .align,
  columns,
  fns,
  fn_names,
  group_keys,
  name_prefix
)
```

## Arguments

- fns:

  vector of functions, even if it's length 1.

- group_keys:

  the keys to group by. likely `epi_keys` (without `time_value`)
