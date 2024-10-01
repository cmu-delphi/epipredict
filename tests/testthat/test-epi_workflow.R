test_that("postprocesser was evaluated", {
  r <- epi_recipe(covid_case_death_rates)
  s <- parsnip::linear_reg()
  f <- frosting()

  ef <- epi_workflow(r, s, f)
  ef2 <- epi_workflow(r, s) %>% add_frosting(f)

  expect_true(epipredict:::has_postprocessor(ef))
  expect_true(epipredict:::has_postprocessor(ef2))
})


test_that("outcome of the two methods are the same", {
  jhu <- covid_case_death_rates

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

  expect_equal(ef, ef2)
})

test_that("model can be added/updated/removed from epi_workflow", {
  jhu <- covid_case_death_rates %>%
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
  expect_snapshot(error = TRUE, extract_spec_parsnip(wf))
  expect_equal(wf$fit$actions$model$spec, NULL)
})

test_that("forecast method works", {
  jhu <- covid_case_death_rates %>%
    filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_naomit()
  wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
  expect_equal(
    forecast(wf),
    predict(wf, new_data = get_test_data(
      hardhat::extract_preprocessor(wf),
      jhu
    ))
  )

  args <- list(
    fill_locf = TRUE,
    n_recent = 360 * 3,
    forecast_date = as.Date("2024-01-01")
  )
  expect_equal(
    forecast(wf, !!!args),
    predict(wf, new_data = get_test_data(
      hardhat::extract_preprocessor(wf),
      jhu,
      !!!args
    ))
  )
})

test_that("forecast method errors when workflow not fit", {
  jhu <- covid_case_death_rates %>%
    filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_naomit()
  wf <- epi_workflow(r, parsnip::linear_reg())

  expect_snapshot(error = TRUE, forecast(wf))
})

test_that("fit method does not silently drop the class", {
  # This is issue #363

  library(recipes)
  tbl <- tibble::tibble(
    geo_value = 1,
    time_value = 1:100,
    x = 1:100,
    y = x + rnorm(100L)
  )
  edf <- as_epi_df(tbl)

  rec_tbl <- recipe(y ~ x, data = tbl)
  rec_edf <- recipe(y ~ x, data = edf)
  expect_snapshot(error = TRUE, epi_recipe(y ~ x, data = tbl))
  erec_edf <- epi_recipe(y ~ x, data = edf)

  ewf_rec_tbl <- epi_workflow(rec_tbl, linear_reg())
  ewf_rec_edf <- epi_workflow(rec_edf, linear_reg())
  ewf_erec_edf <- epi_workflow(erec_edf, linear_reg())

  # above are all epi_workflows:

  expect_s3_class(ewf_rec_tbl, "epi_workflow")
  expect_s3_class(ewf_rec_edf, "epi_workflow")
  expect_s3_class(ewf_erec_edf, "epi_workflow")

  # but fitting drops the class or generates errors in many cases:

  expect_s3_class(ewf_rec_tbl %>% fit(tbl), "epi_workflow")
  expect_s3_class(ewf_rec_tbl %>% fit(edf), "epi_workflow")
  expect_s3_class(ewf_rec_edf %>% fit(tbl), "epi_workflow")
  expect_s3_class(ewf_rec_edf %>% fit(edf), "epi_workflow")
  expect_snapshot(ewf_erec_edf %>% fit(tbl), error = TRUE)
  expect_s3_class(ewf_erec_edf %>% fit(edf), "epi_workflow")
})
