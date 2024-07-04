test_that("epi_recipe blueprint keeps the class, mold works", {
  bp <- default_epi_recipe_blueprint()
  expect_length(class(bp), 4L)
  expect_s3_class(bp, "default_epi_recipe_blueprint")
  expect_s3_class(hardhat::refresh_blueprint(bp), "default_epi_recipe_blueprint")

  jhu <- case_death_rate_subset
  # expect_s3_class(er_check_is_data_like(jhu), "epi_df")

  r <- recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  bp <- hardhat:::update_blueprint(bp, recipe = r)
  run_mm <- run_mold(bp, data = jhu)
  expect_false(is.factor(run_mm$extras$roles$geo_value$geo_value))
})
