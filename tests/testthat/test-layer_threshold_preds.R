jhu <- case_death_rate_subset %>%
  dplyr::filter(time_value < "2021-03-08", geo_value %in% c("ak", "ca", "ar"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)
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
  expect_equal(round(p$.pred, digits = 3), c(0.180, 0, 0.764))
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
    layer_residual_quantile(probs = c(.1, .9)) %>%
    layer_threshold(.pred, dplyr::starts_with("q"), lower = 0.180, upper = 0.31) %>%
    layer_naomit(.pred)
  wf2 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf2, latest))
  expect_equal(ncol(p), 5L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_equal(round(p$.pred, digits = 3), c(0.180, 0.180, 0.310))
  expect_equal(round(p$q0.1, digits = 3), c(0.180, 0.180, 0.310))
  expect_equal(round(p$q0.9, digits = 3), c(0.310, 0.180, 0.310))
  expect_named(p, c("geo_value", "time_value", ".pred", "q0.1", "q0.9"))
})