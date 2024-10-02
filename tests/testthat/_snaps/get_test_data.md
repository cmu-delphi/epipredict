# expect insufficient training data error

    Code
      get_test_data(recipe = r, x = covid_case_death_rates)
    Condition
      Error in `get_test_data()`:
      ! You supplied insufficient recent data for this recipe.
      ! You need at least 367 days of data,
      ! but `x` contains only 365.

# expect error that geo_value or time_value does not exist

    Code
      get_test_data(recipe = r, x = wrong_epi_df)
    Condition
      Error in `get_test_data()`:
      ! `x` must be an `epi_df`.

# NA fill behaves as desired

    Code
      get_test_data(r, df, "A")
    Condition
      Error in `get_test_data()`:
      ! `fill_locf` must be of type <logical>.

---

    Code
      get_test_data(r, df, TRUE, -3)
    Condition
      Error in `get_test_data()`:
      ! `n_recent` must be a positive integer.

---

    Code
      get_test_data(r, df2, TRUE)
    Condition
      Error in `if (recipes::is_trained(recipe)) ...`:
      ! argument is of length zero

# forecast date behaves

    Code
      get_test_data(r, df, TRUE, forecast_date = 9)
    Condition
      Error in `get_test_data()`:
      ! `forecast_date` must be the same class as `x$time_value`.

---

    Code
      get_test_data(r, df, TRUE, forecast_date = 9L)
    Condition
      Error in `get_test_data()`:
      ! `forecast_date` must be no earlier than `max(x$time_value)`

---

    Code
      get_test_data(r, df, forecast_date = 9L)
    Condition
      Error in `get_test_data()`:
      ! `forecast_date` must be no earlier than `max(x$time_value)`

