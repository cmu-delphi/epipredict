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
