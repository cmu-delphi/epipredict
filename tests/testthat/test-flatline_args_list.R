test_that("flatline_args_list checks inputs", {
  expect_s3_class(flatline_args_list(), c("flat_fcast", "alist"))
  expect_error(flatline_args_list(ahead = c(0, 4)))
  expect_error(flatline_args_list(n_training = c(28, 65)))

  expect_error(flatline_args_list(ahead = -1))
  expect_error(flatline_args_list(ahead = 1.5))
  expect_error(flatline_args_list(n_training = -1))
  expect_error(flatline_args_list(n_training = 1.5))
  expect_error(flatline_args_list(lags = c(-1, 0)))
  expect_error(flatline_args_list(lags = list(c(1:5, 6.5), 2:8)))

  expect_error(flatline_args_list(symmetrize = 4))
  expect_error(flatline_args_list(nonneg = 4))

  expect_error(flatline_args_list(quantile_levels = -.1))
  expect_error(flatline_args_list(quantile_levels = 1.1))
  expect_type(flatline_args_list(quantile_levels = NULL), "list")

  expect_error(flatline_args_list(target_date = "2022-01-01"))
  expect_identical(
    flatline_args_list(target_date = as.Date("2022-01-01"))$target_date,
    as.Date("2022-01-01")
  )

  expect_error(flatline_args_list(n_training_min = "de"))
  expect_error(flatline_args_list(epi_keys = 1))

  # Detect mismatched ahead and target_date - forecast_date difference
  expect_warning(flatline_args_list(
    forecast_date = as.Date("2022-01-01"),
    target_date = as.Date("2022-01-03"),
    ahead = 1L
  ))
})
