test_that("check_enough_train_data", {
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

  # Check both columns have enough data, with geo pooling
  expect_no_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n - 2) %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL)
  )
  # Check one column doesn't have enough data, with geo pooling
  expect_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n - 1) %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL),
    regexp = "The following columns don't have enough data"
  )
  # Check column without enough data doesn't error when not checked, with geo
  # pooling
  expect_no_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(y, n = 2 * n - 1) %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL)
  )
  # Check both columns have enough data, without geo pooling
  expect_no_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(x, y, n = n - 2, epi_keys = "geo_value") %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL)
  )
  # Check one column doesn't have enough data, without geo pooling
  expect_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(x, y, n = n - 1, epi_keys = "geo_value") %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL),
    regexp = "The following columns don't have enough data"
  )
  # Check column without enough data doesn't error when not checked, without geo
  # pooling
  expect_no_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(y, n = n - 1, epi_keys = "geo_value") %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL)
  )
  # Check column with NAs counts the NAs if drop_na=TRUE, without geo pooling
  expect_no_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(x, n = n - 1, epi_keys = "geo_value", drop_na = FALSE) %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL)
  )

  # Sanity check the output of a passing recipe
  expect_no_error(
    p <- epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(x, y, n = 2 * n - 2) %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = NULL)
  )

  expect_equal(nrow(p), 2 * n)
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_named(p, c("time_value", "geo_value", "x", "y"))
  expect_equal(
    p$time_value,
    rep(seq(as.Date("2020-01-01"), by = 1, length.out = n), times = 2)
  )
  expect_equal(p$geo_value, rep(c("ca", "hi"), each = n))

  # Check that the train data has enough data, the test data does not, but
  # the check passes anyway (because it should be applied to training data)
  n_minus <- n - 2
  toy_test_data <- tibble::tibble(
    time_value = rep(
      seq(
        as.Date("2020-01-01"),
        by = 1,
        length.out = n_minus
      ),
      times = 2
    ),
    geo_value = rep(c("ca", "hi"), each = n_minus),
    x = c(1:n_minus, c(1:(n_minus - 2), NA, NA)),
    y = 1:(2 * n_minus)
  ) %>% epiprocess::as_epi_df()
  expect_no_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(y, n = n - 1, epi_keys = "geo_value") %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = toy_test_data)
  )
  # Same thing, but skip = FALSE
  expect_no_error(
    epi_recipe(y ~ x, data = toy_epi_df) %>%
      check_enough_train_data(y, n = n - 1, epi_keys = "geo_value", skip = FALSE) %>%
      recipes::prep(toy_epi_df) %>%
      recipes::bake(new_data = toy_test_data)
  )
})
