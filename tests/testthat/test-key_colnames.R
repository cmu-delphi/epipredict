test_that("Extracts keys from a recipe; roles are NA, giving an empty vector", {
  expect_equal(key_colnames(recipe(case_death_rate_subset)), character(0L))
})

test_that("epi_keys_mold extracts time_value and geo_value, but not raw", {
  my_recipe <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_epi_naomit()

  my_workflow <- epi_workflow() %>%
    add_epi_recipe(my_recipe) %>%
    add_model(linear_reg()) %>%
    fit(data = case_death_rate_subset)

  expect_setequal(
    key_colnames(my_workflow$pre$mold),
    c("time_value", "geo_value")
  )
})

test_that("key_colnames extracts additional keys when they are present", {
  my_data <- tibble::tibble(
    geo_value = rep(c("ca", "fl", "pa"), each = 3),
    time_value = rep(seq(as.Date("2020-06-01"), as.Date("2020-06-03"),
      by = "day"
    ), length.out = length(geo_value)),
    pol = rep(c("blue", "swing", "swing"), each = 3), # extra key
    state = rep(c("ca", "fl", "pa"), each = 3), # extra key
    value = 1:length(geo_value) + 0.01 * rnorm(length(geo_value))
  ) %>%
    as_epi_df(
      additional_metadata = list(other_keys = c("state", "pol"))
    )

  my_recipe <- epi_recipe(my_data) %>%
    step_epi_ahead(value, ahead = 7) %>%
    step_epi_naomit()

  my_workflow <- epi_workflow(my_recipe, linear_reg()) %>% fit(my_data)

  expect_setequal(
    key_colnames(my_workflow$pre$mold),
    c("time_value", "geo_value", "state", "pol")
  )
})
