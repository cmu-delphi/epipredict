
test_that("postprocesser was evaluated", {
  r <- epi_recipe(case_death_rate_subset)
  s <- parsnip::linear_reg()
  f <- frosting()

  ef <- epi_workflow(r, s, f)
  ef2 <- epi_workflow(r, s) %>% add_frosting(f)

  expect_true(epipredict:::has_postprocessor(ef))
  expect_true(epipredict:::has_postprocessor(ef2))
})


test_that("outcome of the two methods are the same", {
  jhu <- case_death_rate_subset

  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(case_rate, lag = c(7)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes())

  s <- parsnip::linear_reg()
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantile()

  ef <- epi_workflow(r, s, f)
  ef2 <- epi_workflow(r, s) %>% add_frosting(f)

  expect_equal(ef,ef2)
})
