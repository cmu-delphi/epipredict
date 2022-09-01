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

  expect_error(arx_args_list(target_date = "2022-01-01"))
  expect_identical(
    arx_args_list(target_date = as.Date("2022-01-01"))$target_date,
    as.Date("2022-01-01")
  )
})

