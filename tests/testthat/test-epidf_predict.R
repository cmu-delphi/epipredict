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
                 "Set forecast_date equal to maximum time_value.")
})

test_that("max time_value <= specified forecast_date", {
  # More precisely, max time_value <= as_of <= specified forecast_date
  pred_epi_df2 <- suppressWarnings(epidf_predict(obj, newdata, forecast_date = "2020-04-17"))
  expect_true(is_epi_df(pred_epi_df2))
  expect_length(pred_epi_df2, 3L)
  expect_identical(pred_epi_df2$geo_value, "ca")
  expect_identical(pred_epi_df2$time_value, as.Date("2020-04-17"))
  expect_warning(epidf_predict(obj, newdata, forecast_date = "2020-04-17"),
                 "Maximum time_value is less than or equal to forecast_date.")
})

test_that("specified forecast_date < max_time_value < as_of_date", {
  pred_epi_df3 <- suppressWarnings(epidf_predict(obj, newdata, forecast_date = "2020-04-08"))
  expect_true(is_epi_df(pred_epi_df3))
  expect_length(pred_epi_df3, 3L)
  expect_identical(pred_epi_df3$geo_value, "ca")
  expect_identical(pred_epi_df3$time_value, as.Date("2020-04-08"))
  expect_warning(epidf_predict(obj, newdata, forecast_date = "2020-04-08"),
                 "forecast_date is less than the most recent update date of the data.")
})

test_that("max time_value < specified forecast_date < as_of_date", {
  pred_epi_df4 <- suppressWarnings(epidf_predict(obj, newdata, forecast_date = "2020-04-11"))
  expect_true(is_epi_df(pred_epi_df4))
  expect_length(pred_epi_df4, 3L)
  expect_identical(pred_epi_df4$geo_value, "ca")
  expect_identical(pred_epi_df4$time_value, as.Date("2020-04-11"))
  df4_warn <- capture_warnings(epidf_predict(obj, newdata, forecast_date = "2020-04-11"))
  expect_match(df4_warn[1],
               "Maximum time_value is less than or equal to forecast_date.")
  expect_match(df4_warn[2],
               "forecast_date is less than the most recent update date of the data.")
})
