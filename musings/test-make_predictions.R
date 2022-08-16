library(dplyr)

test_that("prediction works on lm", {
  n <- 100
  p <- 10
  keys <- rep(letters[1:2], each = 50)
  time_var <- rep(1:50, times = 2)
  dat <- data.frame(x = matrix(rnorm(n*p), nrow=n))
  dat$y <- rnorm(n)
  obj <- lm(y ~ ., data = dat)
  dat <- bind_cols(dat, keys = keys, time_value = time_var)
  # two values, 1 key
  expect_length(
    make_predictions(obj, dat, time_var, keys),
    2L)
  # two values, 2 keys
  keys <- data.frame(a=keys, b=rep(rep(letters[1:2], each=25), times=2))
  time_var <- rep(1:25, times = 4)
  dat <- data.frame(x = matrix(rnorm(n*p), nrow=n))
  dat$y <- rnorm(n)
  dat <- bind_cols(dat, keys, time_value = time_var)
  expect_length(
    make_predictions(obj, dat, time_var, keys),
    4L)
})
