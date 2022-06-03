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

y <- tibble(geo_value = rep("nowhere",200),
            time_value = as.Date("2021-01-01") + 0:199,
            case_rate = rpois(100,20) + 1:200,
            death_rate = rpois(100,10) + 1:200) %>%
  as_epi_df()

# Tests
test_that("Check that epi_ahead shifts properly", {
  r1 <- epi_recipe(x) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0,7,14))

  r2 <- epi_recipe(y) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0,7,14))

  r3 <- r1 %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  r4 <- step_naomit2(r2)

  expect_identical(r3,r4)
})
