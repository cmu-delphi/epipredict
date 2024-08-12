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

test_that("check_enough_train_data works on pooled data", {
  # Check both columns have enough data
  expect_no_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n, drop_na = FALSE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
  # Check both column don't have enough data
  expect_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n + 1, drop_na = FALSE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL),
    regexp = "The following columns don't have enough data"
  )
  # Check drop_na works
  expect_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n - 1, drop_na = TRUE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
})

test_that("check_enough_train_data works on unpooled data", {
  # Check both columns have enough data
  expect_no_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = n, epi_keys = "geo_value", drop_na = FALSE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
  # Check one column don't have enough data
  expect_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = n + 1, epi_keys = "geo_value", drop_na = FALSE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL),
    regexp = "The following columns don't have enough data"
  )
  # Check drop_na works
  expect_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n - 3, epi_keys = "geo_value", drop_na = TRUE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
})

test_that("check_enough_train_data outputs the correct recipe values", {
  expect_no_error(
    p <- recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n - 2) %>%
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

test_that("check_enough_train_data only checks train data", {
  # Check that the train data has enough data, the test data does not, but
  # the check passes anyway (because it should be applied to training data)
  toy_test_data <- toy_epi_df %>%
    group_by(geo_value) %>%
    slice(3:10) %>%
    epiprocess::as_epi_df()
  expect_no_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(x, y, n = n - 2, epi_keys = "geo_value") %>%
      prep(toy_epi_df) %>%
      bake(new_data = toy_test_data)
  )
  # Same thing, but skip = FALSE
  expect_no_error(
    recipe(toy_epi_df) %>%
      check_enough_train_data(y, n = n - 2, epi_keys = "geo_value", skip = FALSE) %>%
      prep(toy_epi_df) %>%
      bake(new_data = toy_test_data)
  )
})

test_that("check_enough_train_data works with all_predictors() downstream of constructed terms", {
  # With a lag of 2, we will get 2 * n - 6 non-NA rows
  expect_no_error(
    recipe(toy_epi_df) %>%
      step_epi_lag(x, lag = c(1, 2)) %>%
      check_enough_train_data(all_predictors(), y, n = 2 * n - 6) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
  expect_error(
    recipe(toy_epi_df) %>%
      step_epi_lag(x, lag = c(1, 2)) %>%
      check_enough_train_data(all_predictors(), y, n = 2 * n - 5) %>%
      prep(toy_epi_df) %>%
      bake(new_data = NULL)
  )
})
