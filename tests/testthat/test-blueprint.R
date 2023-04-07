test_that("epi_recipe blueprint keeps the class, mold works", {
  bp <- new_default_epi_recipe_blueprint()
  expect_length(class(bp), 5L)
  expect_s3_class(bp, "default_epi_recipe_blueprint")
  expect_s3_class(refresh_blueprint(bp), "default_epi_recipe_blueprint")

  jhu <- case_death_rate_subset
  # expect_s3_class(er_check_is_data_like(jhu), "epi_df")

  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  mm <- mold_epi_recipe_default_clean(bp, jhu)
  expect_s3_class(mm$blueprint, "default_epi_recipe_blueprint")
  expect_s3_class(mm$data, "epi_df")

  bp <- hardhat:::update_blueprint(bp, recipe = r)
  run_mm <- run_mold(bp, data = jhu)
  expect_false(is.factor(run_mm$extras$roles$geo_value$geo_value))

})
