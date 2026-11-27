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
  expect_warning(p2 <- forecast(wf2), "grouping keys")

  pivot1 <- pivot_quantiles_wider(p1, .pred_distn) %>%
    mutate(width = `0.95` - `0.05`)
  pivot2 <- pivot_quantiles_wider(p2, .pred_distn) %>%
    mutate(width = `0.95` - `0.05`)
  expect_equal(pivot1$width, rep(pivot1$width[1], nrow(pivot1)))
  expect_false(all(pivot2$width == pivot2$width[1]))
})

test_that("by_key uses each key's own residuals with flatline", {
  out <- flatline_forecaster(
    jhu, "death_rate",
    args_list = flatline_args_list(
      quantile_levels = c(0.1, 0.9), nonneg = FALSE,
      quantile_by_key = "geo_value"
    )
  )
  res <- residuals(workflows::extract_fit_engine(out$epi_workflow))
  expected <- res %>%
    dplyr::group_by(geo_value) %>%
    dplyr::summarise(width = diff(quantile(
      c(.resid, -.resid), c(0.1, 0.9),
      na.rm = TRUE, names = FALSE
    )))
  widths <- out$predictions %>%
    pivot_quantiles_wider(.pred_distn) %>%
    dplyr::transmute(geo_value, width = `0.9` - `0.1`)
  expect_equal(
    widths$width,
    expected$width[match(widths$geo_value, expected$geo_value)]
  )
})

test_that("by_key matches residual quantiles to predictions by key", {
  # ak has no forecast because its latest predictors are missing
  jhu_lag <- jhu %>%
    dplyr::filter(!(geo_value == "ak" & time_value > max(time_value) - 3))
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(by_key = "geo_value")
  wf_lag <- epi_workflow(r, parsnip::linear_reg(), f) %>% fit(jhu_lag)
  expect_warning(p <- forecast(wf_lag), "grouping keys")
  expect_equal(p$geo_value, c("ca", "ny"))
  expect_false(anyNA(as.matrix(p$.pred_distn)))
})

test_that("by_key names the keys without usable residuals", {
  # tx has too few observations for a single 7-day flatline residual
  short <- covid_case_death_rates %>%
    dplyr::filter(geo_value == "tx", time_value > max(jhu$time_value) - 3)
  jhu_short <- dplyr::bind_rows(jhu, short) %>% as_epi_df()
  expect_snapshot(
    error = TRUE,
    flatline_forecaster(jhu_short, "death_rate", args_list = flatline_args_list(
      quantile_by_key = "geo_value"
    ))
  )
})

test_that("by_key warns for keys that were not in training", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(by_key = "geo_value")
  wf_no_ak <- epi_workflow(r, parsnip::linear_reg(), f) %>%
    fit(dplyr::filter(jhu, geo_value != "ak"))
  expect_warning(
    expect_warning(
      p <- predict(wf_no_ak, get_test_data(r, jhu)),
      "no training residuals"
    ),
    "grouping keys"
  )
  expect_true(all(is.na(as.matrix(p$.pred_distn[p$geo_value == "ak"]))))
  expect_false(anyNA(as.matrix(p$.pred_distn[p$geo_value != "ak"])))
})

test_that("by_key warns informatively when requested keys are unavailable", {
  f_some <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(by_key = c("nonexistent", "geo_value"))
  wf_some <- epi_workflow(r, parsnip::linear_reg(), f_some) %>% fit(jhu)
  expect_warning(
    expect_warning(
      predict(wf_some, get_test_data(r, jhu)),
      "Residual grouping key \"nonexistent\" was not found in the training data"
    ),
    "grouping keys"
  )

  f_none <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(by_key = "nonexistent")
  wf_none <- epi_workflow(r, parsnip::linear_reg(), f_none) %>% fit(jhu)
  expect_warning(
    predict(wf_none, get_test_data(r, jhu)),
    "calculated across all observations without grouping"
  )
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
    arx_forecaster(jhu, "death_rate", c("case_rate", "death_rate"), args_list = arx_args_list(check_enough_data_n = 1))
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
