set.seed(335744)
ahead <- 1:6
dat <- create_lags_and_leads(rnorm(100), rnorm(100), c(1,3), ahead, 1:100, NULL)
H <- stats::poly(ahead, degree = 3, simple = TRUE)
test_that("standard smooth matrix multiplication", {
  out <- smooth_and_fit(dat, H, FALSE)
  expect_true(is.list(out))
  expect_length(out, 2L)
  expect_identical(class(out$obj), c("mlm", "lm"))
  expect_identical(dim(coef(out$obj)), c(4L, 3L))
  expect_length(out$dat, 9)
})

test_that("kronecker version fails", {
  expect_error(smooth_and_fit(dat, H, TRUE))
})
