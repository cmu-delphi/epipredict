# check_enough_train_data works on pooled data

    Code
      epi_recipe(toy_epi_df) %>% check_enough_train_data(x, y, n = 2 * n + 1,
      drop_na = FALSE) %>% prep(toy_epi_df) %>% bake(new_data = NULL)
    Condition
      Error in `prep()`:
      ! The following columns don't have enough data to predict: x and y.

---

    Code
      epi_recipe(toy_epi_df) %>% check_enough_train_data(x, y, n = 2 * n - 1,
      drop_na = TRUE) %>% prep(toy_epi_df) %>% bake(new_data = NULL)
    Condition
      Error in `prep()`:
      ! The following columns don't have enough data to predict: x and y.

# check_enough_train_data works on unpooled data

    Code
      epi_recipe(toy_epi_df) %>% check_enough_train_data(x, y, n = n + 1, epi_keys = "geo_value",
      drop_na = FALSE) %>% prep(toy_epi_df) %>% bake(new_data = NULL)
    Condition
      Error in `prep()`:
      ! The following columns don't have enough data to predict: x and y.

---

    Code
      epi_recipe(toy_epi_df) %>% check_enough_train_data(x, y, n = 2 * n - 3,
      epi_keys = "geo_value", drop_na = TRUE) %>% prep(toy_epi_df) %>% bake(new_data = NULL)
    Condition
      Error in `prep()`:
      ! The following columns don't have enough data to predict: x and y.

# check_enough_train_data works with all_predictors() downstream of constructed terms

    Code
      epi_recipe(toy_epi_df) %>% step_epi_lag(x, lag = c(1, 2)) %>%
        check_enough_train_data(all_predictors(), y, n = 2 * n - 5) %>% prep(
        toy_epi_df) %>% bake(new_data = NULL)
    Condition
      Error in `prep()`:
      ! The following columns don't have enough data to predict: lag_1_x, lag_2_x, and y.

