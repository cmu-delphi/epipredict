test_that("smooth_arx_args checks inputs", {
  expect_error(smooth_arx_args_list(ahead = c(0, 4)))
  expect_error(smooth_arx_args_list(min_train_window = c(28, 65)))

  expect_error(smooth_arx_args_list(ahead = -1))
  expect_error(smooth_arx_args_list(ahead = 1.5))
  expect_error(smooth_arx_args_list(min_train_window = -1))
  expect_error(smooth_arx_args_list(min_train_window = 1.5))
  expect_error(smooth_arx_args_list(lags = c(-1, 0)))
  expect_error(smooth_arx_args_list(lags = list(c(1:5,6.5), 2:8)))

  expect_error(smooth_arx_args_list(symmetrize = 4))
  expect_error(smooth_arx_args_list(nonneg = 4))

  expect_error(smooth_arx_args_list(levels = -.1))
  expect_error(smooth_arx_args_list(levels = 1.1))
  expect_type(smooth_arx_args_list(levels = NULL), "list")
})


test_that("smooth_arx returns proper empty tibble", {
  template1 <- tibble::tibble(
    key_vars = rep(1:10, times = 4),
    point = NA)
  template1 <- enframer(template1, c("q0.05", "q0.95"))
  template1$ahead <- rep(c(1L, 2L, 4L, 7L), each = 10)
  template1 <- template1 %>% relocate(ahead)
  expect_identical(
    smooth_arx_forecaster(
      1:100, 1:10, key_vars = 1:10, 1:10,
      smooth_arx_args_list(ahead = c(1L,2L,4L,7L), degree=2)),
    template1
  )
  names(template1)[2] = "aaa"
  expect_identical(
    smooth_arx_forecaster(
      1:100, 1:10, key_vars = tibble(aaa=1:10), 1:10,
      smooth_arx_args_list(ahead = c(1L,2L,4L,7L), degree=2)),
    template1
  )
})

test_that("simple forms produce output", {
  x <- rnorm(200)
  y <- rnorm(200)
  out1 <- smooth_arx_forecaster(x, y, NULL, 1:200)
  expect_equal(nrow(out1), 28L)
  expect_named(out1, c("ahead", "point", "q0.05", "q0.95"))

  out2 <- smooth_arx_forecaster(x, y, rep(letters[1:2], each=100), rep(1:100, times=2))
  expect_equal(nrow(out2), 56L)
  expect_named(out2, c("ahead", "key_vars", "point", "q0.05", "q0.95"))

  out3 <- smooth_arx_forecaster(x, y,
                         data.frame(geo_value = rep(letters[1:2], each=100)),
                         rep(1:100, times=2),
                         smooth_arx_args_list(levels = c(.5, .8, .9)))
  expect_equal(nrow(out3), 56L)
  expect_named(out3, c("ahead", "geo_value", "point", "q0.5", "q0.8", "q0.9"))
})
