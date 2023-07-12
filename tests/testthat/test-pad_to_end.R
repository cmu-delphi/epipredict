test_that("test set padding works", {
  dat <- tibble::tibble(
    gr1 = rep(c("a", "b"), times = c(3, 4)),
    time_value = c(1:3, 1:4),
    value = 1:7
  )
  expect_identical(pad_to_end(dat, "gr1", 3), dat)
  expect_equal(nrow(pad_to_end(dat, "gr1", 4)), 8L)
  p <- pad_to_end(dat, "gr1", 5)
  expect_equal(nrow(p), 10L)
  expect_identical(p$gr1, rep(c("a", "b"), times = 5))
  expect_identical(p$time_value, rep(1:5, each = 2))
  expect_identical(p$value, as.integer(c(1, 4, 2, 5, 3, 6, NA, 7, NA, NA)))

})
