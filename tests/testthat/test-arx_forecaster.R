train_data <- epidatasets::cases_deaths_subset
test_that("arx_forecaster warns if forecast date beyond the implicit one", {
  bad_date <- max(train_data$time_value) + 300
  expect_warning(
    arx1 <- arx_forecaster(
      train_data,
      "death_rate_7d_av",
      c("death_rate_7d_av", "case_rate_7d_av"),
      args_list = (arx_args_list(forecast_date = bad_date))
    ),
    class = "epipredict__arx_forecaster__forecast_date_defaulting"
  )
})

test_that("arx_forecaster errors if forecast date, target date, and ahead are inconsistent", {
  max_date <- max(train_data$time_value)
  expect_error(
    arx1 <- arx_forecaster(
      train_data,
      "death_rate_7d_av",
      c("death_rate_7d_av", "case_rate_7d_av"),
      args_list = (arx_args_list(ahead = 5, target_date = max_date, forecast_date = max_date))
    ),
    class = "epipredict__arx_args__inconsistent_target_ahead_forecaste_date"
  )
})
