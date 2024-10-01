test_that("bake method works in all cases", {
  edf <- case_death_rate_subset %>%
    filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
  r <- epi_recipe(edf) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7)

  r2 <- epi_recipe(edf) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_naomit()

  b_null <- bake(prep(r, edf), NULL)
  b_train <- bake(prep(r, edf), edf)
  expect_s3_class(b_null, "epi_df")
  expect_identical(b_null, b_train)

  b_baked <- bake(prep(r2, edf), edf) # leaves rows with NA in the response
  # doesn't (because we "juice", so skip doesn't apply)
  b_juiced <- bake(prep(r2, edf), NULL)
  expect_equal(nrow(b_juiced), sum(complete.cases(b_train)))
  expect_equal(nrow(b_baked), sum(complete.cases(b_train)) + 3 * 7)

  # check that the {recipes} behaves
  expect_s3_class(bake(prep(r, edf), NULL, composition = "tibble"), "tbl_df")
  expect_s3_class(bake(prep(r, edf), NULL, composition = "data.frame"), "data.frame")
  # can't be a matrix because time_value/geo_value aren't numeric
  expect_snapshot(error = TRUE, bake(prep(r, edf), NULL, composition = "matrix"))
})
