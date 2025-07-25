jhu <- covid_case_death_rates %>%
  dplyr::filter(time_value < "2021-03-08", geo_value %in% c("ak", "ca", "ar"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- jhu %>%
  dplyr::filter(time_value >= max(time_value) - 14)

test_that("Default pred_lower and pred_upper work as intended", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred)
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 3L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(round(p$.pred, digits = 3), c(0.179, 0, 0.765))
  # expect_named(p, c("time_value", "geo_value", ".pred"))
  expect_named(p, c("geo_value", "time_value", ".pred"))
})

test_that("Specified pred_lower and pred_upper work as intended", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred, lower = 0.180, upper = 0.31) %>%
    layer_naomit(.pred)
  wf2 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf2, latest))
  expect_equal(ncol(p), 3L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(round(p$.pred, digits = 3), c(0.180, 0.180, 0.310))
  expect_named(p, c("geo_value", "time_value", ".pred"))
})

test_that("thresholds additional columns", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_residual_quantiles(quantile_levels = c(.1, .9)) %>%
    layer_threshold(.pred, .pred_distn, lower = 0.180, upper = 0.31) %>%
    layer_naomit(.pred)

  wf2 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf2, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(round(p$.pred, digits = 3), c(0.180, 0.180, 0.310))
  expect_named(p, c("geo_value", "time_value", ".pred", ".pred_distn"))
  p <- p %>%
    pivot_quantiles_longer(.pred_distn)
  expect_equal(
    round(p$.pred_distn_value, digits = 3),
    c(0.180, 0.180, 0.31, 0.180, .18, .18, .31, 0.310, .31)
  )
  expect_equal(p$.pred_distn_quantile_level, rep(c(.1, 0.5, .9), times = 3))
})
