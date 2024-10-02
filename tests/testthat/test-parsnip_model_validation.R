test_that("forecaster can validate parsnip model", {
  l <- list()
  trainer1 <- parsnip::linear_reg()
  trainer2 <- parsnip::logistic_reg()
  trainer3 <- parsnip::rand_forest()

  expect_snapshot(error = TRUE, get_parsnip_mode(l))
  expect_equal(get_parsnip_mode(trainer1), "regression")
  expect_equal(get_parsnip_mode(trainer2), "classification")
  expect_equal(get_parsnip_mode(trainer3), "unknown")

  expect_snapshot(error = TRUE, is_classification(l))
  expect_true(is_regression(trainer1))
  expect_false(is_classification(trainer1))
  expect_true(is_classification(trainer2))
  expect_false(is_regression(trainer2))
  expect_true(is_regression(trainer3))
  expect_true(is_classification(trainer3))
})
