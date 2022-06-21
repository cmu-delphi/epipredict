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

f <- epipredict:::frosting() %>%
      layer_naomit(death_rate)

wf1 <- wf %>% add_frosting(f)

test_that("Removing NA works", {
  expect_silent(p <- predict(wf1, latest))
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 83L)
  expect_named(p, c("geo_value", "time_value","case_rate","death_rate"))
})
