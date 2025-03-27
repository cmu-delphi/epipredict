# Setup toy data
n <- 10
toy_epi_df <- tibble::tibble(
  time_value = rep(
    seq(
      as.Date("2020-01-01"),
      by = 1,
      length.out = n
    ),
    times = 2
  ),
  geo_value = rep(c("ca", "hi"), each = n),
  x = c(1:n, c(1:(n - 2), NA, NA)),
  y = 1:(2 * n)
) %>% epiprocess::as_epi_df()

test_that("check_enough_data works on pooled data", {
  # Check both columns have enough data
  expect_no_error(
    epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = 2 * n, drop_na = FALSE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
  # Check both column don't have enough data
  expect_snapshot(
    error = TRUE,
    epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = 2 * n + 1, drop_na = FALSE) %>%
      prep(toy_epi_df)
  )
  # Check drop_na works
  expect_snapshot(
    error = TRUE,
    epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = 2 * n - 1, drop_na = TRUE) %>%
      prep(toy_epi_df)
  )
})

test_that("check_enough_data works on unpooled data", {
  # Check both columns have enough data
  expect_no_error(
    epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = n, epi_keys = "geo_value", drop_na = FALSE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
  # Check one column don't have enough data
  expect_snapshot(
    error = TRUE,
    epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = n + 1, epi_keys = "geo_value", drop_na = FALSE) %>%
      prep(toy_epi_df)
  )
  # Check drop_na works
  expect_snapshot(
    error = TRUE,
    epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = 2 * n - 3, epi_keys = "geo_value", drop_na = TRUE) %>%
      prep(toy_epi_df)
  )
})

test_that("check_enough_data outputs the correct recipe values", {
  expect_no_error(
    p <- epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = 2 * n - 2) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )

  expect_equal(nrow(p), 2 * n)
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_named(p, c("geo_value", "time_value", "x", "y")) # order in epiprocess::new_epi_df
  expect_equal(
    p$time_value,
    rep(seq(as.Date("2020-01-01"), by = 1, length.out = n), times = 2)
  )
  expect_equal(p$geo_value, rep(c("ca", "hi"), each = n))
})

test_that("check_enough_data only checks train data when skip = FALSE", {
  # Check that the train data has enough data, the test data does not, but
  # the check passes anyway (because it should be applied to training data)
  toy_test_data <- toy_epi_df %>%
    group_by(geo_value) %>%
    slice(3:10) %>%
    epiprocess::as_epi_df()
  expect_no_error(
    epi_recipe(toy_epi_df) %>%
      check_enough_data(x, y, min_data_points = n - 2, epi_keys = "geo_value") %>%
      prep(toy_epi_df) %>%
      bake(new_data = toy_test_data)
  )
  # Making sure `skip = TRUE` is working correctly in `predict`
  expect_no_error(
    epi_recipe(toy_epi_df) %>%
      add_role(y, new_role = "outcome") %>%
      check_enough_data(x, min_data_points = n - 2, epi_keys = "geo_value") %>%
      epi_workflow(linear_reg()) %>%
      fit(toy_epi_df) %>%
      predict(new_data = toy_test_data %>% filter(time_value > "2020-01-08"))
  )
  # making sure it works for skip = FALSE, where there's enough data to train
  # but not enough to predict
  expect_no_error(
    forecaster <- epi_recipe(toy_epi_df) %>%
      add_role(y, new_role = "outcome") %>%
      check_enough_data(x, min_data_points = 1, epi_keys = "geo_value", skip = FALSE) %>%
      epi_workflow(linear_reg()) %>%
      fit(toy_epi_df)
  )
  expect_snapshot(
    error = TRUE,
    forecaster %>%
      predict(new_data = toy_test_data %>% filter(time_value > "2020-01-08"))
  )
})

test_that("check_enough_data works with all_predictors() downstream of constructed terms", {
  # With a lag of 2, we will get 2 * n - 5 non-NA rows (NA's in x but not in the
  # lags don't count)
  expect_no_error(
    epi_recipe(toy_epi_df) %>%
      step_epi_lag(x, lag = c(1, 2)) %>%
      check_enough_data(all_predictors(), y, min_data_points = 2 * n - 5) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
  expect_snapshot(
    error = TRUE,
    epi_recipe(toy_epi_df) %>%
      step_epi_lag(x, lag = c(1, 2)) %>%
      check_enough_data(all_predictors(), y, min_data_points = 2 * n - 4) %>%
      prep(toy_epi_df)
  )
})
