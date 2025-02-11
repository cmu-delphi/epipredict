test_that("climate args list validates properly", {
  expect_s3_class(climate_args_list(), c("climate_fcast", "alist"))
  expect_s3_class(
    climate_args_list(forecast_date = as.Date("2021-01-10")),
    c("climate_fcast", "alist")
  )
  expect_snapshot(error = TRUE, climate_args_list(forecast_date = 12345))
  expect_snapshot(
    error = TRUE,
    climate_args_list(forecast_date = as.Date(c("2021-01-10", "2024-01-22")))
  )
  expect_silent(climate_args_list(forecast_horizon = 1L))
  expect_silent(climate_args_list(forecast_horizon = -1:4))
  expect_snapshot(error = TRUE, climate_args_list(forecast_horizon = 1.3))
  expect_snapshot(error = TRUE, climate_args_list(window_size = -1))
  expect_snapshot(error = TRUE, climate_args_list(window_size = 2.5))
  expect_snapshot(error = TRUE, climate_args_list(window_size = 1:3))
  expect_snapshot(error = TRUE, climate_args_list(quantile_levels = -1))
  expect_snapshot(error = TRUE, climate_args_list(quantile_levels = 1.3))
  expect_snapshot(error = TRUE, climate_args_list(symmetrize = 2.5))
  expect_snapshot(error = TRUE, climate_args_list(symmetrize = c(TRUE, TRUE)))
  expect_snapshot(error = TRUE, climate_args_list(nonneg = 2.5))
  expect_snapshot(error = TRUE, climate_args_list(nonneg = c(TRUE, TRUE)))
  expect_snapshot(error = TRUE, climate_args_list(quantile_by_key = TRUE))
  expect_snapshot(error = TRUE, climate_args_list(quantile_by_key = 2:3))
})


