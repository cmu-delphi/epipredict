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
    layer_residual_quantiles(quantile_levels = c(0.0275, 0.8, 0.95), symmetrize = FALSE)

  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_named(p, c("geo_value", "time_value", ".pred", ".pred_distn"))

  nested <- p %>% dplyr::mutate(.quantiles = nested_quantiles(.pred_distn))
  unnested <- nested %>% tidyr::unnest(.quantiles)

  expect_equal(nrow(unnested), 9L)
  expect_equal(unique(unnested$quantile_levels), c(.0275, .8, .95))
})


test_that("Errors when used with a classifier", {
  tib <- tibble(
    y = factor(rep(c("a", "b"), length.out = 100)),
    x1 = rnorm(100),
    x2 = rnorm(100),
    time_value = 1:100,
    geo_value = "ak"
  ) %>% as_epi_df()

  r <- epi_recipe(y ~ x1 + x2, data = tib)
  wf <- epi_workflow(r, parsnip::logistic_reg()) %>% fit(tib)
  f <- frosting() %>%
    layer_predict() %>%
    layer_residual_quantiles()
  wf <- wf %>% add_frosting(f)
  expect_error(predict(wf, tib))
})


test_that("Grouping by keys is supported", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles()
  wf1 <- wf %>% add_frosting(f)
  expect_silent(p1 <- predict(wf1, latest))
  f2 <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(by_key = "geo_value")
  wf2 <- wf %>% add_frosting(f2)
  expect_warning(p2 <- predict(wf2, latest))

  pivot1 <- pivot_quantiles_wider(p1, .pred_distn) %>%
    mutate(width = `0.95` - `0.05`)
  pivot2 <- pivot_quantiles_wider(p2, .pred_distn) %>%
    mutate(width = `0.95` - `0.05`)
  expect_equal(pivot1$width, rep(pivot1$width[1], nrow(pivot1)))
  expect_false(all(pivot2$width == pivot2$width[1]))
})
