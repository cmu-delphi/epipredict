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

# Preparing the datasets to be used for comparison
r <- epi_recipe(x) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(death_rate, lag = c(0,7,14))

z1 <- step_naomit2(r)
z2 <- r %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)

# Test
test_that("Check that both functions behave the same way", {
  expect_identical(z1$steps[[3]][-1][-5],z2$steps[[3]][-1][-5])
  expect_identical(z1$steps[[4]][-1][-5],z2$steps[[4]][-1][-5])
  expect_identical(class(z1$steps[[3]]),class(z2$steps[[3]]))
  expect_identical(class(z1$steps[[4]]),class(z2$steps[[4]]))
})
