install.packages("parsnip")
install.packages("workflows")

library(dplyr)
library(epiprocess)
library(parsnip)
library(workflows)

# Random generated dataset
set.seed(100)
x <- tibble(geo_value = rep("nowhere",200),
              time_value = as.Date("2021-01-01") + 0:199,
              case_rate = rpois(100,20) + 1:200,
              death_rate = rpois(100,10) + 1:200) %>%
  as_epi_df()

slm_fit <- function(recipe, data = x) {
  workflow() %>%
    add_recipe(recipe) %>%
    add_model(linear_reg()) %>%
    fit(data = data)
}

# Tests
test_that("Check that epi_ahead shifts properly", {
  r1 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = -7) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  slm_fit1 <- slm_fit(r1)

  slope_ahead <- slm_fit1$fit$fit$fit$coefficients[[2]]
  expect_equal(slope_ahead,1)
})

test_that("Check that epi_lag shifts properly", {
  r2 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = -7) %>%
    step_epi_lag(death_rate, lag = 7) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  slm_fit2 <- slm_fit(r2)

  slope_lag <- slm_fit2$fit$fit$fit$coefficients[[2]]
  expect_equal(slope_lag,1)
})

test_that("Check for non-integer values", {
  r3 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 3.6) %>%
    step_epi_lag(death_rate, lag = 1.9)
  expect_error(
    slm_fit(r3)
  )
})

test_that("Check for duplicate values", {
  r4 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = 7) %>%
    step_epi_lag(death_rate, lag = 7)
  expect_error(
    slm_fit(r4)
  )
})
