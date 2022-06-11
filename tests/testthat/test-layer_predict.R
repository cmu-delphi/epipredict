test_that("predict layer works alone", {
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

  f <- frosting() %>% layer_predict()
  wf <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf, latest))
  expect_equal(ncol(p), 3L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 108L)
  expect_named(p, c("geo_value", "time_value", ".pred"))

})
