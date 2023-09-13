library(dplyr)
library(parsnip)
library(workflows)

# Random generated dataset
x <- tibble(
  geo_value = rep("nowhere", 200),
  time_value = as.Date("2021-01-01") + 0:199,
  case_rate = 1:200,
  death_rate = 1:200
) %>%
  epiprocess::as_epi_df()

# Preparing the datasets to be used for comparison
r <- epi_recipe(x) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14))

test_that("Argument must be a recipe", {
  expect_error(step_epi_naomit(x))
})

z1 <- step_epi_naomit(r)
z2 <- r %>%
  step_naomit(all_predictors(), skip = FALSE) %>%
  step_naomit(all_outcomes(), skip = TRUE)

# Checks the behaviour of a step function, omitting the quosure and id that
# differ from one another, even with identical behaviour
behav <- function(recipe, step_num) recipe$steps[[step_num]][-1][-5]
# Checks the class type of an object
step_class <- function(recipe, step_num) class(recipe$steps[step_num])

test_that("Check that both functions behave the same way", {
  expect_identical(behav(z1, 3), behav(z2, 3))
  expect_identical(behav(z1, 4), behav(z2, 4))
  expect_identical(step_class(z1, 3), step_class(z2, 3))
  expect_identical(step_class(z1, 4), step_class(z2, 4))
})
