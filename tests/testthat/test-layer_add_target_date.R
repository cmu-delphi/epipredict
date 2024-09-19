jhu <- covid_case_death_rates %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)
wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- jhu %>%
  dplyr::filter(time_value >= max(time_value) - 14)

test_that("Use ahead + max time value from pre, fit, post", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_add_target_date() %>%
    layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(p$target_date, rep(as.Date("2022-01-07"), times = 3))
  expect_named(p, c("geo_value", "time_value", ".pred", "target_date"))

  # Should be same dates as above
  f2 <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date() %>%
    layer_add_target_date() %>%
    layer_naomit(.pred)
  wf2 <- wf %>% add_frosting(f2)

  # expect_warning(p2 <- predict(wf2, latest)) # this warning has been removed
  expect_silent(p2 <- predict(wf2, latest))
  expect_equal(ncol(p2), 5L)
  expect_s3_class(p2, "epi_df")
  expect_equal(nrow(p2), 3L)
  expect_equal(p2$target_date, rep(as.Date("2022-01-07"), times = 3))
  expect_named(p2, c("geo_value", "time_value", ".pred", "forecast_date", "target_date"))
})

test_that("Use ahead + specified forecast date", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date(forecast_date = "2022-05-31") %>%
    layer_add_target_date() %>%
    layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 5L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(p$target_date, rep(as.Date("2022-06-07"), times = 3))
  expect_named(p, c("geo_value", "time_value", ".pred", "forecast_date", "target_date"))
})

test_that("Specify own target date", {
  # No forecast date layer
  f <- frosting() %>%
    layer_predict() %>%
    layer_add_target_date(target_date = "2022-01-08") %>%
    layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p1 <- predict(wf1, latest))
  expect_equal(ncol(p1), 4L)
  expect_s3_class(p1, "epi_df")
  expect_equal(nrow(p1), 3L)
  expect_equal(p1$target_date, rep(as.Date("2022-01-08"), times = 3))
  expect_named(p1, c("geo_value", "time_value", ".pred", "target_date"))

  # Include forecast date layer - should be same results as previous
  f2 <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date() %>%
    layer_add_target_date(target_date = "2022-01-08") %>%
    layer_naomit(.pred)
  wf2 <- wf %>% add_frosting(f2)

  expect_silent(p2 <- predict(wf1, latest))
  expect_equal(ncol(p2), 4L)
  expect_s3_class(p2, "epi_df")
  expect_equal(nrow(p2), 3L)
  expect_equal(p2$target_date, rep(as.Date("2022-01-08"), times = 3))
  expect_named(p2, c("geo_value", "time_value", ".pred", "target_date"))
})

test_that("target date works for daily and yearly", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_add_target_date() %>%
    layer_naomit(.pred)

  wf1 <- add_frosting(wf, f)
  p <- predict(wf1, latest)
  # both target_date and epi_df are dates
  expect_identical(p$target_date[1], as.Date("2021-12-31") + 7L)

  # the error happens at predict time because the
  # time_value train/test types don't match
  latest_bad <- latest %>%
    unclass() %>%
    as.data.frame() %>%
    mutate(time_value = as.POSIXlt(time_value)$year + 1900L) %>%
    as_epi_df()
  expect_error(predict(wf1, latest_bad))

  # target_date is a string (gets correctly converted to Date)
  wf1 <- add_frosting(
    wf,
    adjust_frosting(f, "layer_add_target_date", target_date = "2022-01-07")
  )
  expect_silent(predict(wf1, latest))

  # target_date is a year/int while the epi_df is a date
  wf1 <- add_frosting(
    wf,
    adjust_frosting(f, "layer_add_target_date", target_date = 2022L)
  )
  expect_error(predict(wf1, latest)) # wrong time type of forecast_date
})
