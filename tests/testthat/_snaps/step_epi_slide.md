# epi_slide works on weekly data with one of before/ahead set

    Code
      baked
    Output
      An `epi_df` object, 40 x 4 with metadata:
      * geo_type  = state
      * time_type = week
      * as_of     = 1999-09-09
      
      # A tibble: 40 x 4
         geo_value time_value value epi_slide__.f_value
       * <chr>     <date>     <int>               <dbl>
       1 ca        2022-01-01     2                 2  
       2 ca        2022-01-08     3                 2.5
       3 ca        2022-01-15     4                 3  
       4 ca        2022-01-22     5                 3.5
       5 ca        2022-01-29     6                 4.5
       6 ca        2022-02-05     7                 5.5
       7 ca        2022-02-12     8                 6.5
       8 ca        2022-02-19     9                 7.5
       9 ca        2022-02-26    10                 8.5
      10 ca        2022-03-05    11                 9.5
      # i 30 more rows

