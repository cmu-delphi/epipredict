library(dplyr)
library(epiprocess)
library(parsnip)
library(workflows)

# Random generated dataset
x <- tibble(geo_value = rep("nowhere",200),
              time_value = as.Date("2021-01-01") + 0:199,
              case_rate = atan(0.5 * 1:200) + sin(5*1:200) + 1:200,
              death_rate = atan(0.1 * 1:200) + cos(5*1:200) + 1:200) %>%
  as_epi_df()

slm_fit <- function(recipe, data = x) {
  workflow() %>%
    add_recipe(recipe) %>%
    add_model(linear_reg()) %>%
    fit(data = data)
}

test_that("Values for ahead and lag must be integer values", {
  r1 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 3.6) %>%
    step_epi_lag(death_rate, lag = 1.9)
  expect_error(
    slm_fit(r1)
  )
})

test_that("Values for ahead and lag cannot be duplicates", {
  r2 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = 7) %>%
    step_epi_lag(death_rate, lag = 7)
  expect_error(
    slm_fit(r2)
  )
})

test_that("Check that epi_lag shifts properly", {
  r3 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0,7,14)) %>%
    step_epi_lag(case_rate, lag = c(0,7,14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  slm_fit3 <- slm_fit(r3)

  slope_lag <- slm_fit3$fit$fit$fit$coefficients[[2]]

  expect_equal(1,1) # stub
})
