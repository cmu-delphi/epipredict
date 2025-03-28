# check_enough_data works on pooled data

    Code
      epi_recipe(toy_epi_df) %>% check_enough_data(x, y, min_observations = 2 * n + 1,
      drop_na = FALSE) %>% prep(toy_epi_df)
    Condition
      Error in `check_enough_data_core()`:
      ! The following columns don't have enough data to train: x and y.

---

    Code
      epi_recipe(toy_epi_df) %>% check_enough_data(x, y, min_observations = 2 * n - 1,
      drop_na = TRUE) %>% prep(toy_epi_df)
    Condition
      Error in `check_enough_data_core()`:
      ! The following columns don't have enough data to train: x.

# check_enough_data works on unpooled data

    Code
      epi_recipe(toy_epi_df) %>% check_enough_data(x, y, min_observations = n + 1,
      epi_keys = "geo_value", drop_na = FALSE) %>% prep(toy_epi_df)
    Condition
      Error in `check_enough_data_core()`:
      ! The following columns don't have enough data to train: x and y.

---

    Code
      epi_recipe(toy_epi_df) %>% check_enough_data(x, y, min_observations = 2 * n - 3,
      epi_keys = "geo_value", drop_na = TRUE) %>% prep(toy_epi_df)
    Condition
      Error in `check_enough_data_core()`:
      ! The following columns don't have enough data to train: x and y.

# check_enough_data only checks train data when skip = FALSE

    Code
      forecaster %>% predict(new_data = toy_test_data %>% filter(time_value >
        "2020-01-08"))
    Condition
      Error in `check_enough_data_core()`:
      ! The following columns don't have enough data to predict: x.

# check_enough_data works with all_predictors() downstream of constructed terms

    Code
      epi_recipe(toy_epi_df) %>% step_epi_lag(x, lag = c(1, 2)) %>% check_enough_data(
        all_predictors(), y, min_observations = 2 * n - 4) %>% prep(toy_epi_df)
    Condition
      Error in `check_enough_data_core()`:
      ! The following columns don't have enough data to train: no single column, but the combination of lag_1_x, lag_2_x, y.

