# pad every group at the right interval

Perform last observation carried forward on a group by group basis. It
uses `guess_period` to find the appropriate interval to fill-forward by.
It maintains the grouping structure it recieves. It does *not* fill any
"interior" `NA` values occurring in the data beforehand.

## Usage

``` r
pad_to_end(x, groups, end_date, columns_to_complete = NULL)
```

## Arguments

- x:

  an epi_df to be filled forward.

- groups:

  the grouping by which to fill forward

- columns_to_complete:

  which columns to apply completion to. By default every non-key column
  of an epi_df
