library(dplyr)
train <- jhu_csse_daily_subset |>
  filter(time_value >= as.Date("2021-10-01")) |>
  select(geo_value, time_value, cr = case_rate_7d_av, dr = death_rate_7d_av)
ngeos <- n_distinct(train$geo_value)

test_that("flatline determines target_date where forecast_date exists", {
  flat <- flatline_forecaster(
    train, "dr",
    args_list = flatline_args_list(
      forecast_date = as.Date("2021-12-31"),
      target_date = as.Date("2022-01-01"),
      ahead = 1L
    )
  )

  # previously, if target_date existed, it could be
  # erroneously incremented by the ahead
  expect_identical(
    flat$predictions$target_date,
    rep(as.Date("2022-01-01"), ngeos)
  )
  expect_identical(
    flat$predictions$forecast_date,
    rep(as.Date("2021-12-31"), ngeos)
  )

  # potentially resulted in NA predictions
  # see #290 https://github.com/cmu-delphi/epipredict/issues/290
  expect_true(all(!is.na(flat$predictions$.pred_distn)))
  expect_true(all(!is.na(flat$predictions$.pred)))
})

test_that("arx_forecaster determines target_date where forecast_date exists", {
  arx <- arx_forecaster(
    train, "dr", c("dr", "cr"),
    args_list = arx_args_list(
      forecast_date = as.Date("2021-12-31"),
      target_date = as.Date("2022-01-01"),
      ahead = 1L
    )
  )
  # previously, if target_date existed, it could be
  # erroneously incremented by the ahead
  expect_identical(
    arx$predictions$target_date,
    rep(as.Date("2022-01-01"), ngeos)
  )
  expect_identical(
    arx$predictions$forecast_date,
    rep(as.Date("2021-12-31"), ngeos)
  )

  # potentially resulted in NA predictions
  # see #290 https://github.com/cmu-delphi/epipredict/issues/290
  expect_true(all(!is.na(arx$predictions$.pred_distn)))
  expect_true(all(!is.na(arx$predictions$.pred)))
})

test_that("arx_classifier determines target_date where forecast_date exists", {
  arx <- arx_classifier(
    train, "dr", c("dr"),
    trainer = parsnip::nearest_neighbor(mode = "classification"),
    args_list = arx_class_args_list(
      forecast_date = as.Date("2021-12-31"),
      target_date = as.Date("2022-01-01"),
      ahead = 1L
    )
  )

  # previously, if target_date existed, it could be
  # erroneously incremented by the ahead
  expect_identical(
    arx$predictions$target_date,
    rep(as.Date("2022-01-01"), ngeos)
  )
  expect_identical(
    arx$predictions$forecast_date,
    rep(as.Date("2021-12-31"), ngeos)
  )

  # potentially resulted in NA predictions
  # see #290 https://github.com/cmu-delphi/epipredict/issues/290
  expect_true(all(!is.na(arx$predictions$.pred_class)))
})
