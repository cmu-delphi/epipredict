test_that("layer argument extractor works", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_residual_quantiles(quantile_levels = c(0.0275, 0.975), symmetrize = FALSE) %>%
    layer_naomit(.pred)

  expect_snapshot(error = TRUE, extract_argument(f$layers[[1]], "uhoh", "bubble"))
  expect_snapshot(error = TRUE, extract_argument(f$layers[[1]], "layer_predict", "bubble"))
  expect_identical(
    extract_argument(f$layers[[2]], "layer_residual_quantiles", "quantile_levels"),
    c(0.0275, 0.9750)
  )

  expect_snapshot(error = TRUE, extract_argument(f, "layer_thresh", "quantile_levels"))
  expect_identical(
    extract_argument(f, "layer_residual_quantiles", "quantile_levels"),
    c(0.0275, 0.9750)
  )

  wf <- epi_workflow(postprocessor = f)
  expect_snapshot(error = TRUE, extract_argument(epi_workflow(), "layer_residual_quantiles", "quantile_levels"))
  expect_identical(
    extract_argument(wf, "layer_residual_quantiles", "quantile_levels"),
    c(0.0275, 0.9750)
  )

  expect_snapshot(error = TRUE, extract_argument(wf, "layer_predict", c("type", "opts")))
})

test_that("recipe argument extractor works", {
  jhu <- case_death_rate_subset %>%
    dplyr::filter(time_value > "2021-08-01") %>%
    dplyr::arrange(geo_value, time_value)

  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    # below, `skip` means we don't do this at predict time
    step_naomit(all_outcomes(), skip = TRUE)


  expect_snapshot(error = TRUE, extract_argument(r$steps[[1]], "uhoh", "bubble"))
  expect_snapshot(error = TRUE, extract_argument(r$steps[[1]], "step_epi_lag", "bubble"))
  expect_identical(extract_argument(r$steps[[2]], "step_epi_ahead", "ahead"), 7L)


  expect_snapshot(error = TRUE, extract_argument(r, "step_lightly", "quantile_levels"))
  expect_identical(
    extract_argument(r, "step_epi_lag", "lag"),
    list(c(0L, 7L, 14L), c(0L, 7L, 14L))
  )

  wf <- epi_workflow(preprocessor = r)
  expect_snapshot(error = TRUE, extract_argument(epi_workflow(), "step_epi_lag", "lag"))
  expect_identical(
    extract_argument(wf, "step_epi_lag", "lag"),
    list(c(0L, 7L, 14L), c(0L, 7L, 14L))
  )
})
