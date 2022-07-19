jhu <- case_death_rate_subset %>%
 dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
 step_epi_lag(death_rate, lag = c(0, 7, 14, 30)) %>%
 step_epi_ahead(death_rate, ahead = 7) %>%
 recipes::step_naomit(all_predictors()) %>%
 recipes::step_naomit(all_outcomes(), skip = TRUE)

wf <- epipredict::epi_workflow(r, parsnip::linear_reg()) %>% parsnip::fit(jhu)

latest <- get_test_data(recipe = r, x = jhu) # 93 x 4
latest[1:10, 4] <- NA # 10 rows have NA


test_that("Removing NA after predict", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred)

  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 2L) # ak is NA so removed
  expect_named(p, c("geo_value", "time_value",".pred"))
})


