test_that("arx_args checks inputs", {
  expect_error(arx_args_list(ahead = c(0, 4)))
  expect_error(arx_args_list(min_train_window = c(28, 65)))

  expect_error(arx_args_list(ahead = -1))
  expect_error(arx_args_list(ahead = 1.5))
  expect_error(arx_args_list(min_train_window = -1))
  expect_error(arx_args_list(min_train_window = 1.5))
  expect_error(arx_args_list(lags = c(-1, 0)))
  expect_error(arx_args_list(lags = list(c(1:5,6.5), 2:8)))

  expect_error(arx_args_list(symmetrize = 4))
  expect_error(arx_args_list(nonneg = 4))

  expect_error(arx_args_list(levels = -.1))
  expect_error(arx_args_list(levels = 1.1))
  expect_type(arx_args_list(levels = NULL), "list")
})


test_that("arx returns proper empty tibble",{
  template1 <- data.frame(key_vars = 1:10, point = NA)
  template1 <- enframer(template1, c("q0.05", "q0.95"))
  expect_identical(
    arx_forecaster(1:100, 1:10, key_vars = 1:10, 1:10),
    template1
  )
  names(template1)[1] = "aaa"
  expect_identical(
    arx_forecaster(1:100, 1:10, data.frame(aaa = 1:10), 1:10),
    template1
  )
})
