set.seed(2034)
n <- 100
tib <- tibble(
  x = rnorm(n), y = x + rnorm(n),
  time_value = seq(as.Date("2020-01-01"), by = 1, length.out = n),
  geo_value = "ca"
) %>% epiprocess::as_epi_df(as_of = "2020-04-12")

obj <- lm(y ~ x, data = tib)
newdata <- tib %>%
  slice_tail(n = 1)

test_that("ahead specified and forecast_date = NULL", {
  pred_epi_df <- suppressWarnings(epidf_predict(obj, newdata, ahead = 7))
  expect_true(is_epi_df(pred_epi_df))
  expect_length(pred_epi_df, 3L)
  expect_identical(pred_epi_df$geo_value, "ca")
  expect_identical(pred_epi_df$time_value, as.Date("2020-04-16"))
  expect_warning(epidf_predict(obj, newdata, ahead = 7),
                 "Set forecast_date equal to maximum time value plus ahead value.")
})

test_that("max_time_value < forecast_date = as_of", {
  pred_epi_df2 <- epidf_predict(obj, newdata, forecast_date = "2020-04-12")
  expect_true(is_epi_df(pred_epi_df2))
  expect_length(pred_epi_df2, 3L)
  expect_identical(pred_epi_df2$geo_value, "ca")
  expect_identical(pred_epi_df2$time_value, as.Date("2020-04-12"))
})

test_that("max_time_value < as_of < forecast_date", {
  pred_epi_df3 <- epidf_predict(obj, newdata, forecast_date = "2020-04-14")
  expect_true(is_epi_df(pred_epi_df3))
  expect_length(pred_epi_df3, 3L)
  expect_identical(pred_epi_df3$geo_value, "ca")
  expect_identical(pred_epi_df3$time_value, as.Date("2020-04-14"))
})

test_that("forecast_date < max_time_value < as_of", {
  pred_epi_df4 <- suppressWarnings(epidf_predict(obj, newdata, forecast_date = "2020-04-08"))
  expect_true(is_epi_df(pred_epi_df4))
  expect_length(pred_epi_df4, 3L)
  expect_identical(pred_epi_df4$geo_value, "ca")
  expect_identical(pred_epi_df4$time_value, as.Date("2020-04-08"))
  expect_warning(epidf_predict(obj, newdata, forecast_date = "2020-04-08"),
                 "forecast_date is less than the most recent update date of the data.")
})

test_that("max time_value < forecast_date < as_of", {
  pred_epi_df5 <- suppressWarnings(epidf_predict(obj, newdata, forecast_date = "2020-04-11"))
  expect_true(is_epi_df(pred_epi_df5))
  expect_length(pred_epi_df5, 3L)
  expect_identical(pred_epi_df5$geo_value, "ca")
  expect_identical(pred_epi_df5$time_value, as.Date("2020-04-11"))
  expect_warning(epidf_predict(obj, newdata, forecast_date = "2020-04-11"),
                 "forecast_date is less than the most recent update date of the data.")
})
