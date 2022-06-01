library(dplyr)
library(covidcast)
library(delphi.epidata)
library(epiprocess)
library(tidymodels)

# Taken from example-recipe
x <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200301, 20211231),
  geo_values = "*"
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, case_rate = value)

y <- covidcast(
  data_source = "jhu-csse",
  signals = "deaths_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200301, 20211231),
  geo_values = "*"
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, death_rate = value)

x <- x %>%
  full_join(y, by = c("geo_value", "time_value")) %>%
  as_epi_df()
rm(y)

xx <- x %>% filter(time_value > "2021-12-01")

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
