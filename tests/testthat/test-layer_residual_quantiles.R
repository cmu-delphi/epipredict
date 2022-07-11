jhu <- case_death_rate_subset %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- get_test_data(recipe = r, x = jhu)


test_that("Returns expected number or rows and columns", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(probs = c(0.0275, 0.8, 0.95), symmetrize = FALSE)

  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_named(p, c("geo_value", "time_value",".pred",".pred_distn"))

  nested <- p %>% dplyr::mutate(.quantiles = nested_quantiles(.pred_distn))
  unnested <- nested %>% tidyr::unnest(.quantiles)

  expect_equal(nrow(unnested), 9L)
  expect_equal(unique(unnested$tau), c(.0275, .8, .95))
})
