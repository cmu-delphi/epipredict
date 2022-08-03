library(parsnip)
library(workflows)
library(dplyr)

test_that("epi_keys returns null for an object that isn't an epi_df",{
  expect_null(epi_keys(data.frame(x=1:3,y=2:4)))
})

test_that("Extracts keys from an epi_df",{
  expect_equal(epi_keys(case_death_rate_subset),
                c("time_value","geo_value"))
})

test_that("Extracts keys from a recipe; roles are NA, giving an empty vector",{
  expect_equal(epi_keys(recipe(case_death_rate_subset)),c("")[-1])
})

test_that("epi_keys_mold extracts time_value and geo_value, but not raw",{
  my_recipe <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0,7,14)) %>%
    step_epi_lag(case_rate, lag = c(0,7,14)) %>%
    step_epi_naomit()

  my_workflow <- epi_workflow() %>%
    add_epi_recipe(my_recipe) %>%
    add_model(linear_reg()) %>%
    fit(data = case_death_rate_subset)

  expect_equal(epi_keys_mold(my_workflow$pre$mold),
               c("time_value","geo_value"))
})

test_that("epi_keys_mold extracts additional keys when they are present", {
  my_data <- case_death_rate_subset %>%
    sample_n(6) %>%
    tsibble::as_tsibble() %>% # add 2 extra keys
    mutate(state = rep("MA", 6), pol = rep("blue", 6)) %>% 
    as_epi_df(additional_metadata = list(other_keys=c("state", "pol")))
  
  my_recipe <- epi_recipe(my_data) %>%
    step_epi_ahead(death_rate , ahead = 7) %>%
    step_epi_naomit()

  my_workflow <- epi_workflow(my_recipe, linear_reg()) %>% fit(my_data)

  expect_equal(
    epi_keys_mold(my_workflow$pre$mold), 
    c("time_value", "geo_value", "state", "pol"))
})
