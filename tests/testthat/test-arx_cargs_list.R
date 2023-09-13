test_that("arx_class_args checks inputs", {
  expect_s3_class(arx_class_args_list(), c("arx_class", "alist"))
  expect_error(arx_class_args_list(ahead = c(0, 4)))
  expect_error(arx_class_args_list(n_training = c(28, 65)))

  expect_error(arx_class_args_list(ahead = -1))
  expect_error(arx_class_args_list(ahead = 1.5))
  expect_error(arx_class_args_list(n_training = -1))
  expect_error(arx_class_args_list(n_training = 1.5))
  expect_error(arx_class_args_list(lags = c(-1, 0)))
  expect_error(arx_class_args_list(lags = list(c(1:5, 6.5), 2:8)))


  expect_error(arx_class_args_list(target_date = "2022-01-01"))
  expect_identical(
    arx_class_args_list(target_date = as.Date("2022-01-01"))$target_date,
    as.Date("2022-01-01")
  )
})
