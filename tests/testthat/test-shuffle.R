test_that("shuffle works", {
  expect_error(shuffle(matrix(NA, 2, 2)))
  expect_length(shuffle(1:10), 10L)
  expect_identical(sort(shuffle(1:10)), 1:10)
})
