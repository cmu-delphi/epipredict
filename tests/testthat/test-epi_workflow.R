
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
    step_epi_naomit()

  s <- parsnip::linear_reg()
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles()

  ef <- epi_workflow(r, s, f)
  ef2 <- epi_workflow(r, s) %>% add_frosting(f)

  expect_equal(ef,ef2)
})

test_that("model can be added/updated/removed from epi_workflow", {
  jhu <- case_death_rate_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7)

  rf_model <- rand_forest(mode = "regression")

  wf <- epi_workflow(r)

  wf <- wf %>% add_model(rf_model)
  model_spec <- extract_spec_parsnip(wf)
  expect_equal(model_spec$engine, "ranger")
  expect_equal(model_spec$mode, "regression")
  expect_equal(class(model_spec), c("rand_forest", "model_spec"))

  lm_model <- parsnip::linear_reg()

  wf <- update_model(wf, lm_model)
  model_spec2 <- extract_spec_parsnip(wf)
  expect_equal(model_spec2$engine, "lm")
  expect_equal(model_spec2$mode, "regression")
  expect_equal(class(model_spec2), c("linear_reg", "model_spec"))

  wf <- remove_model(wf)
  expect_error(extract_spec_parsnip(wf))
  expect_equal(wf$fit$actions$model$spec, NULL)

})
