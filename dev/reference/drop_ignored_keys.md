# given a list named by key columns, remove any matching key values keys_to_ignore should have the form list(col_name = c("value_to_ignore", "other_value_to_ignore"))

given a list named by key columns, remove any matching key values
keys_to_ignore should have the form list(col_name = c("value_to_ignore",
"other_value_to_ignore"))

## Usage

``` r
drop_ignored_keys(training, keys_to_ignore)
```
