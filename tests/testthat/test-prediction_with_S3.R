test_that("prediction works on lm", {
  n <- 100
  p <- 10
  keys <- rep(letters[1:2], each = 50)
  time_var <- rep(1:50, times = 2)
  dat <- data.frame(matrix(rnorm(n*p), nrow=n))
  dat$y <- rnorm(n)
  obj <- lm(y ~ ., data = dat)
  # two values, 1 key
  expect_length(
    make_prediction_with_S3(obj, dat, keys, time_var),
    2L)
  # two values, 2 keys
  keys <- data.frame(a=keys, b=rep(rep(letters[1:2], each=25), times=2))
  time_var <- rep(1:25, times = 4)
  expect_length(
    make_prediction_with_S3(obj, dat, keys, time_var),
    4L)
})
