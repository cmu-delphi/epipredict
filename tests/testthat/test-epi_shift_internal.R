# Loading
library(tidyverse)
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

# Tests
test_that("Check that epi_ahead shifts properly", {
  r <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = -7) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  slm_fit <- workflow() %>%
    add_recipe(r) %>%
    add_model(linear_reg()) %>%
    fit(data = x)

  slope_ahead <- slm_fit$fit$fit$fit$coefficients[[2]]
  expect_equal(slope_ahead,1)
})

test_that("Check that epi_lag shifts properly", {
  r2 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = -7) %>%
    step_epi_lag(death_rate, lag = 7) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  slm_fit2 <- workflow() %>%
    add_recipe(r2) %>%
    add_model(linear_reg()) %>%
    fit(data = x)

  slope_lag <- slm_fit2$fit$fit$fit$coefficients[[2]]
  expect_equal(slope_lag,1)
})
