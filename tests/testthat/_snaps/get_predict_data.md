# expect insufficient training data error when the forecast date is unreasonable

    Code
      get_predict_data(recipe = r, x = covid_case_death_rates)
    Condition
      Error in `get_predict_data()`:
      ! predict data is filtered to no rows; check your `predict_interval = 365` and `reference_date= 2023-03-10`

# expect error that geo_value or time_value does not exist

    Code
      get_predict_data(recipe = r, x = wrong_epi_df)
    Condition
      Error in `get_predict_data()`:
      ! `x` must be an `epi_df`.

