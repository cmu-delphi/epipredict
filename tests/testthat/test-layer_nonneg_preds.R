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


test_that("Default pred_lower and pred_upper work as intended", {

  f <- frosting() %>% layer_predict() %>% layer_nonneg_preds()
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 3L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_named(p, c("time_value", "geo_value", ".pred"))
})

test_that("Specified pred_lower and pred_upper work as intended", {

  f <- frosting() %>% layer_predict() %>% layer_nonneg_preds(pred_lower = 0.25, pred_upper = 0.31)
  wf2 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf2, latest))
  expect_equal(ncol(p), 3L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 1L)
  expect_named(p, c("time_value", "geo_value", ".pred"))
})
