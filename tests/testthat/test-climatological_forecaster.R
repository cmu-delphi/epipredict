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

test_that("climatological_forecaster works as expected", {
  single_yr <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
    y = rep(c(1:183, 184:2), times = 2L)
  ) %>%
    as_epi_df(as_of = max(single_yr))
  clim_forecast <- climatological_forecaster(x, "y", args_list = climate_args_list(time_type = "day"))
  preds <- clim_forecast$predictions %>%
    mutate(
      quant_med = median(.pred_distn)
    )
  expect_equal(preds$.pred, preds$quant_med)

  expected_res <- tibble(
    geo_value = rep(c("reg1", "reg2"), 5),
    forecast_date = as.Date("2020-12-31"),
    target_date = c(
      rep(as.Date("2020-12-31"), 2), rep(as.Date("2021-01-01"), 2), rep(as.Date("2021-01-02"), 2), rep(as.Date("2021-01-03"), 2), rep(as.Date("2021-01-04"), 2)
    ),
    .pred = c(rep(3, 8), rep(4, 2))
  )
  expect_equal(preds %>% select(geo_value, forecast_date, target_date, .pred), expected_res)
})
