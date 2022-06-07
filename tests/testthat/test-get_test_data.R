test_that("return expected number of rows", {
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14, 21, 28)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  test <- get_test_data(recipe = r, x = case_death_rate_subset)

  expect_equal(nrow(test),
               dplyr::n_distinct(case_death_rate_subset$geo_value)* 29)
})


test_that("expect insufficient training data error", {
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 367)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  expect_error(get_test_data(recipe = r, x = case_death_rate_subset))
})

test_that("expect error that geo_value or time_value does not exist", {
  r <-  epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  wrong_epi_df <- case_death_rate_subset %>% dplyr::select(-geo_value)

  expect_error(get_test_data(recipe = r, x = wrong_epi_df))
})
