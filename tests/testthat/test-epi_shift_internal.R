library(dplyr)
library(epiprocess)
library(parsnip)
library(workflows)

# Random generated dataset
x <- tibble(geo_value = rep("place",200),
              time_value = as.Date("2021-01-01") + 0:199,
              case_rate = sqrt(1:200) + atan(0.1 * 1:200) + sin(5*1:200) + 1,
              death_rate = atan(0.1 * 1:200) + cos(5*1:200) + 1) %>%
  as_epi_df()

slm_fit <- function(recipe, data = x) {
  workflow() %>%
    add_recipe(recipe) %>%
    add_model(linear_reg()) %>%
    fit(data = data)
}

test_that("Values for ahead and lag must be integer values", {
  expect_error(
    r1 <- epi_recipe(x) %>%
      step_epi_ahead(death_rate, ahead = 3.6) %>%
      step_epi_lag(death_rate, lag = 1.9)
  )
})

test_that("A negative lag value should should throw an error", {
  expect_error(
    r2 <- epi_recipe(x) %>%
      step_epi_ahead(death_rate, ahead = 7) %>%
      step_epi_lag(death_rate, lag = -7)
  )
})

test_that("A nonpositive ahead value should throw an error", {
  expect_error(
    r3 <- epi_recipe(x) %>%
      step_epi_ahead(death_rate, ahead = -7) %>%
      step_epi_lag(death_rate, lag = 7)
  )
})

test_that("Values for ahead and lag cannot be duplicates", {
  r4 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = 7) %>%
    step_epi_lag(death_rate, lag = 7)
  expect_error(
    slm_fit(r4)
  )
})

test_that("Check that epi_lag shifts applies the shift", {
  r5 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0,7,14))

  # Two steps passed here
  expect_equal(length(r5$steps),2)
  fit5 <- slm_fit(r5)

  # Should have four predictors, including the intercept
  expect_equal(length(fit5$fit$fit$fit$coefficients),4)
})
