train_data <- epidatasets::cases_deaths_subset
test_that("arx_forecaster warns if forecast date beyond the implicit one", {
  bad_date <- max(train_data$time_value) + 300
  expect_error(
    expect_warning(
      arx1 <- arx_forecaster(
        train_data,
        "death_rate_7d_av",
        c("death_rate_7d_av", "case_rate_7d_av"),
        args_list = (arx_args_list(forecast_date = bad_date))
      ),
      class = "epipredict__arx_forecaster__forecast_date_defaulting"
    ),
    class = "epipredict__get_predict_data__no_predict_data")
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

test_that("warns if there's not enough data to predict", {
  edf <- tibble(
    geo_value = "ct",
    time_value = seq(as.Date("2020-10-01"), as.Date("2023-05-31"), by = "day"),
  ) %>%
    mutate(value = seq_len(nrow(.)) + rnorm(nrow(.))) %>%
    # Oct to May (flu season, ish) only:
    filter(!dplyr::between(as.POSIXlt(time_value)$mon + 1L, 6L, 9L)) %>%
    # and actually, pretend we're around mid-October 2022:
    filter(time_value <= as.Date("2022-10-12")) %>%
    as_epi_df(as_of = as.Date("2022-10-12"))
  edf %>% filter(time_value > "2022-08-01")

  expect_error(
    edf %>% arx_forecaster("value"),
    class = "epipredict__not_enough_data"
  )
})
