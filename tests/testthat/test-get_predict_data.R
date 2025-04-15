suppressPackageStartupMessages(library(dplyr))
forecast_date <- max(covid_case_death_rates$time_value)
test_that("return expected number of rows for various `test_intervals`", {
  r <- epi_recipe(covid_case_death_rates, reference_date = forecast_date) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14, 21, 28)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  predict_data <- get_predict_data(recipe = r, x = covid_case_death_rates)

  expect_equal(
    nrow(predict_data),
    dplyr::n_distinct(covid_case_death_rates$geo_value) * 365
  )

  predict_data <- get_predict_data(recipe = r, test_interval = 5, x = covid_case_death_rates)

  expect_equal(
    nrow(predict_data),
    dplyr::n_distinct(covid_case_death_rates$geo_value) * 5
  )

  predict_data <- get_predict_data(recipe = r, test_interval = as.difftime(35, units = "days"), x = covid_case_death_rates)

  expect_equal(
    nrow(predict_data),
    dplyr::n_distinct(covid_case_death_rates$geo_value) * 35
  )
})


test_that("expect insufficient training data error when the forecast date is unreasonable", {
  r <- epi_recipe(covid_case_death_rates) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 367)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  expect_snapshot(error = TRUE, get_predict_data(recipe = r, x = covid_case_death_rates))
})


test_that("expect error that geo_value or time_value does not exist", {
  r <- epi_recipe(covid_case_death_rates, reference_date = forecast_date) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  wrong_epi_df <- covid_case_death_rates %>% dplyr::select(-geo_value)

  expect_snapshot(error = TRUE, get_predict_data(recipe = r, x = wrong_epi_df))
})
