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

test_that("Specify a forecast_date in `layer_add_forecast_date()`", {

  f <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date(forecast_date = "2021-12-31") %>%
    layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(p$forecast_date, rep(as.Date("2021-12-31"), times = 3))
  expect_named(p, c("time_value", "geo_value", ".pred", "forecast_date"))
})

test_that("Fail to specify a forecast_date in `layer_add_forecast_date()`", {

  f <- frosting() %>%
    layer_predict() %>%
    layer_add_forecast_date() %>%
    layer_naomit(.pred)
  wf2 <- wf %>% add_frosting(f)

  expect_error(predict(wf2, latest), "`forecast_date` must be specified")
})
