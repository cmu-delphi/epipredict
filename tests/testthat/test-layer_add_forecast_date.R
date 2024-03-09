jhu <- case_death_rate_subset %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)
wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- jhu %>%
  dplyr::filter(time_value >= max(time_value) - 14)

test_that("layer validation works", {
  f <- frosting()


  expect_error(layer_add_forecast_date(f, "2022-05-31", id = 2))
  expect_silent(layer_add_forecast_date(f, "2022-05-31"))
  expect_silent(layer_add_forecast_date(f))
  expect_silent(layer_add_forecast_date(f, as.Date("2022-05-31")))
  expect_silent(layer_add_forecast_date(f, as.Date("2022-05-31"), id = "a"))
})

test_that("Specify a `forecast_date` that is greater than or equal to `as_of` date", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date(forecast_date = as.Date("2022-05-31")) %>%
    layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(p$forecast_date, rep(as.Date("2022-05-31"), times = 3))
  expect_named(p, c("geo_value", "time_value", ".pred", "forecast_date"))
})

test_that("Specify a `forecast_date` that is less than `as_of` date", {
  f2 <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date(forecast_date = as.Date("2021-12-31")) %>%
    layer_naomit(.pred)
  wf2 <- wf %>% add_frosting(f2)

  # this warning has been removed
  # expect_warning(
  #   p2 <- predict(wf2, latest),
  #   "forecast_date is less than the most recent update date of the data."
  # )
  expect_silent(p2 <- predict(wf2, latest))
  expect_equal(ncol(p2), 4L)
  expect_s3_class(p2, "epi_df")
  expect_equal(nrow(p2), 3L)
  expect_equal(p2$forecast_date, rep(as.Date("2021-12-31"), times = 3))
  expect_named(p2, c("geo_value", "time_value", ".pred", "forecast_date"))
})

test_that("Do not specify a forecast_date in `layer_add_forecast_date()`", {
  f3 <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date() %>%
    layer_naomit(.pred)
  wf3 <- wf %>% add_frosting(f3)

  # this warning has been removed
  # expect_warning(
  #   p3 <- predict(wf3, latest),
  #   "forecast_date is less than the most recent update date of the data."
  # )
  expect_silent(p3 <- predict(wf3, latest))
  expect_equal(ncol(p3), 4L)
  expect_s3_class(p3, "epi_df")
  expect_equal(nrow(p3), 3L)
  expect_equal(p3$forecast_date, rep(as.Date("2021-12-31"), times = 3))
  expect_named(p3, c("geo_value", "time_value", ".pred", "forecast_date"))
})
