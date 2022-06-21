jhu <- case_death_rate_subset %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- get_test_data(recipe = r, x = jhu)


test_that("function works", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantile(probs = c(0.0275, 0.975), symmetrize = FALSE)

  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
})
