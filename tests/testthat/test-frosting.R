test_that("frosting validators / constructors work", {
  wf <- epi_workflow()
  expect_s3_class(new_frosting(), "frosting")
  expect_true(is_frosting(new_frosting()))
  expect_silent(epi_workflow(postprocessor = new_frosting()))
  expect_false(has_postprocessor(wf))
  expect_false(has_postprocessor_frosting(wf))
  expect_silent(wf %>% add_frosting(new_frosting()))
  expect_silent(wf %>% add_postprocessor(new_frosting()))
  expect_error(wf %>% add_postprocessor(list()))

  wf <- wf %>% add_frosting(new_frosting())
  expect_true(has_postprocessor(wf))
  expect_true(has_postprocessor_frosting(wf))
})

test_that("frosting can be created/added/updated/adjusted/removed", {
  f <- frosting()
  expect_error(frosting(layers = 1:5))
  wf <- epi_workflow() %>% add_frosting(f)
  expect_true(has_postprocessor_frosting(wf))
  wf1 <- update_frosting(wf, frosting() %>% layer_predict() %>% layer_threshold(.pred))
  expect_true(has_postprocessor_frosting(wf1))
  expect_equal(length(wf1$post$actions$frosting$frosting$layers), 2)
  wf1 <- adjust_frosting(wf1, which_layer = 2, upper = 1) # adjust frosting by layer number
  expect_true(has_postprocessor_frosting(wf1))
  expect_equal(length(wf1$post$actions$frosting$frosting$layers), 2)
  expect_equal(wf1$post$actions$frosting$frosting$layers[[2]]$upper, 1)
  wf1 <- adjust_frosting(wf1, which_layer = "layer_threshold", upper = 5) # adjust frosting by layer name
  expect_true(has_postprocessor_frosting(wf1))
  expect_equal(length(wf1$post$actions$frosting$frosting$layers), 2)
  expect_equal(wf1$post$actions$frosting$frosting$layers[[2]]$upper, 5)
  wf1 <- wf1 %>% remove_frosting()
  expect_false(has_postprocessor_frosting(wf1))
  expect_false(has_postprocessor(wf1))
  expect_equal(length(wf1$post$actions$frosting$frosting$layers), 0)
  expect_null(wf1$post$actions$frosting$frosting$layers[[1]])
})



test_that("prediction works without any postprocessor", {
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

  expect_silent(predict(wf, latest))
  p <- predict(wf, latest) %>%
    dplyr::filter(!is.na(.pred))
  expect_equal(nrow(p), 3)
  expect_s3_class(p, "epi_df")
  expect_equal(tail(p$time_value, 1), as.Date("2021-12-31"))
  expect_equal(unique(p$geo_value), c("ak", "ca", "ny"))
})


test_that("layer_predict is added by default if missing", {
  jhu <- case_death_rate_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_naomit()

  wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)

  latest <- get_test_data(recipe = r, x = jhu)

  f1 <- frosting() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles()

  f2 <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles()

  wf1 <- wf %>% add_frosting(f1)
  wf2 <- wf %>% add_frosting(f2)

  expect_equal(predict(wf1, latest), predict(wf2, latest))
})
