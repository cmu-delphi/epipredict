library(tidymodels)

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

  my_workflow <- workflow() %>%
    add_recipe(my_recipe) %>%
    add_model(linear_reg()) %>%
    fit(data = case_death_rate_subset)

  expect_equal(epi_keys_mold(my_workflow$pre$mold),
               c("time_value","geo_value"))
})
