jhu <- covid_case_death_rates %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))

r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_naomit()

wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)


test_that("Returns expected number or rows and columns", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(quantile_levels = c(0.0275, 0.8, 0.95), symmetrize = FALSE)

  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- forecast(wf1))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 3L)
  expect_named(p, c("geo_value", "time_value", ".pred", ".pred_distn"))

  unnested <- p %>% pivot_quantiles_longer(.pred_distn)
  expect_equal(nrow(unnested), 12L)
  expect_equal(unique(unnested$.pred_distn_quantile_level), c(.0275, 0.5, .8, .95))
})

test_that("new name works correctly", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(name = "foo")

  wf1 <- wf %>% add_frosting(f)
  expect_equal(names(forecast(wf1)), c("geo_value", "time_value", ".pred", "foo"))

  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(name = "geo_value")

  wf1 <- wf %>% add_frosting(f)
  expect_error(forecast(wf1))
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
  expect_snapshot(error = TRUE, forecast(wf))
})


test_that("Grouping by keys is supported", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles()
  wf1 <- wf %>% add_frosting(f)
  expect_silent(p1 <- forecast(wf1))
  f2 <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(by_key = "geo_value")
  wf2 <- wf %>% add_frosting(f2)
  expect_warning(p2 <- forecast(wf2))

  pivot1 <- pivot_quantiles_wider(p1, .pred_distn) %>%
    mutate(width = `0.95` - `0.05`)
  pivot2 <- pivot_quantiles_wider(p2, .pred_distn) %>%
    mutate(width = `0.95` - `0.05`)
  expect_equal(pivot1$width, rep(pivot1$width[1], nrow(pivot1)))
  expect_false(all(pivot2$width == pivot2$width[1]))
})

test_that("Canned forecasters work with / without", {
  meta <- attr(jhu, "metadata")
  meta$as_of <- max(jhu$time_value)
  attr(jhu, "metadata") <- meta

  expect_silent(
    flatline_forecaster(jhu, "death_rate")
  )
  expect_silent(
    flatline_forecaster(
      jhu, "death_rate",
      args_list = flatline_args_list(quantile_by_key = "geo_value")
    )
  )

  expect_silent(
    arx_forecaster(jhu, "death_rate", c("case_rate", "death_rate"))
  )
  expect_silent(
    flatline_forecaster(
      jhu, "death_rate",
      args_list = flatline_args_list(quantile_by_key = "geo_value")
    )
  )
})

test_that("flatline_forecaster correctly errors when n_training < ahead", {
  expect_snapshot(
    error = TRUE,
    flatline_forecaster(jhu, "death_rate", args_list = flatline_args_list(ahead = 10, n_training = 9))
  )
})
