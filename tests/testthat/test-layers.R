test_that("A layer can be updated in frosting", {
  # Modify lower in `layer_threshold`
  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred)
  fold <- f
  expect_equal(class(f), "frosting")
  expect_equal(length(f$layers), 2)
  expect_equal(f$layers[[2]]$lower, 0)
  f$layers[[2]] <- update(f$layers[[2]], lower = 100)
  expect_equal(length(f$layers), 2)
  expect_equal(f$layers[[1]], fold$layers[[1]])
  expect_equal(f$layers[[2]]$lower, 100)
  expect_error(update(f$layers[[1]], lower = 100))
  expect_error(update(f$layers[[3]], lower = 100))
  expect_error(update(f$layers[[2]], bad_param = 100))
})
