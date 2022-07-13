test_that("layer argument extractor works", {
  f <- frosting() %>%
    layer_predict() %>%
    layer_residual_quantile(probs = c(0.0275, 0.975), symmetrize = FALSE) %>%
    layer_naomit(.pred)

  expect_error(extract_layer_argument(f$layers[[1]], "uhoh", "bubble"))
  expect_error(extract_layer_argument(f$layers[[1]], "layer_predict", "bubble"))
  expect_identical(
    extract_layer_argument(f$layers[[2]], "layer_residual_quantile", "probs"),
    c(0.0275, 0.9750))

  expect_error(extract_layer_argument(f, "layer_thresh", "probs"))
  expect_identical(
    extract_layer_argument(f, "layer_residual_quantile", "probs"),
    c(0.0275, 0.9750))

  wf <- epi_workflow(postprocessor = f)
  expect_error(extract_layer_argument(epi_workflow(), "layer_residual_quantile", "probs"))
  expect_identical(
    extract_layer_argument(wf, "layer_residual_quantile", "probs"),
    c(0.0275, 0.9750))

  expect_error(extract_layer_argument(wf, "layer_predict", c("type", "opts")))
})
