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

test_that("Use ahead from preprocessing", {

  f <- frosting() %>% layer_predict() %>%
    layer_add_target_date() %>% layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(p$target_date, rep(as.Date("2022-01-07"), times = 3))
  expect_named(p, c("geo_value", "time_value", ".pred", "target_date"))
})

test_that("Override default behaviour and specify own target date", {

  f <- frosting() %>% layer_predict() %>%
    layer_add_target_date(target_date = "2022-01-08") %>% layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p2 <- predict(wf1, latest))
  expect_equal(ncol(p2), 4L)
  expect_s3_class(p2, "epi_df")
  expect_equal(nrow(p2), 3L)
  expect_equal(p2$target_date, rep(as.Date("2022-01-08"), times = 3))
  expect_named(p2, c("geo_value", "time_value", ".pred", "target_date"))
})
