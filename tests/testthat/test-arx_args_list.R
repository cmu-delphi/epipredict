test_that("arx_args checks inputs", {
  expect_s3_class(arx_args_list(), c("arx_fcast", "alist"))
  expect_error(arx_args_list(ahead = c(0, 4)))
  expect_error(arx_args_list(n_training = c(28, 65)))

  expect_error(arx_args_list(ahead = -1))
  expect_error(arx_args_list(ahead = 1.5))
  expect_error(arx_args_list(n_training = -1))
  expect_error(arx_args_list(n_training = 1.5))
  expect_error(arx_args_list(lags = c(-1, 0)))
  expect_error(arx_args_list(lags = list(c(1:5, 6.5), 2:8)))

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

test_that("arx forecaster disambiguates quantiles", {
  alist <- eval(formals(arx_args_list)$levels)
  tlist <- eval(formals(quantile_reg)$tau)
  expect_identical( # both default
    compare_quantile_args(alist, tlist),
    sort(c(alist, tlist))
  )
  alist <- c(.5, alist)
  expect_identical( # tlist is default, should give alist
    compare_quantile_args(alist, tlist),
    sort(unique(alist))
  )
  alist <- eval(formals(arx_args_list)$levels)
  tlist <- c(.05, .95, tlist)
  expect_identical( # alist is default, should give tlist
    compare_quantile_args(alist, tlist),
    sort(unique(tlist))
  )
  alist <- c(.3, .5, .7)
  tlist <- c(.3, .5, .5, .7)
  expect_identical( # neither default, but identical, should run
    compare_quantile_args(alist, tlist),
    sort(unique(tlist))
  )
  alist <- c(.1, .3, .5, .7, .9) # neither default, and different,
  expect_error(compare_quantile_args(alist, tlist))
})

test_that("arx_lags_validator handles named & unnamed lists as expected", {
  # Fully named list of lags in order of predictors
  pred_vec <- c("death_rate", "case_rate")
  lags_init_fn <- list(death_rate = c(0, 7, 14), case_rate = c(0, 1, 2, 3, 7, 14))

  expect_equal(
    arx_lags_validator(pred_vec, lags_init_fn),
    lags_init_fn
  )

  # Fully named list of lags not in order of predictors
  lags_finit_fn_switch <- list(case_rate = c(0, 1, 2, 3, 7, 14), death_rate = c(0, 7, 14))

  expect_equal(
    arx_lags_validator(pred_vec, lags_finit_fn_switch),
    list(death_rate = c(0, 7, 14), case_rate = c(0, 1, 2, 3, 7, 14))
  )

  # Fully named list of lags not in order of predictors (longer ex.)
  pred_vec2 <- c("death_rate", "other_var", "case_rate")
  lags_finit_fn_switch2 <- list(
    case_rate = c(0, 1, 2, 3, 7, 14), death_rate = c(0, 7, 14),
    other_var = c(0, 1)
  )
  expect_equal(
    arx_lags_validator(pred_vec2, lags_finit_fn_switch2),
    list(
      death_rate = c(0, 7, 14),
      other_var = c(0, 1), case_rate = c(0, 1, 2, 3, 7, 14)
    )
  )

  # More lags than predictors - Error
  expect_error(arx_lags_validator(pred_vec, lags_finit_fn_switch2))

  # Unnamed list of lags
  lags_init_un <- list(c(0, 7, 14), c(0, 1, 2, 3, 7, 14))

  expect_equal(arx_lags_validator(pred_vec, lags_init_un), lags_init_un)

  # Partially named list of lags - treat as unnamed
  lags_init_pn <- list(death_rate = c(0, 7, 14), c(0, 1, 2, 3, 7, 14))

  expect_equal(arx_lags_validator(pred_vec, lags_init_pn), lags_init_pn)

  # NA name - treat as unnamed list
  lags_init_na <- list(c(0, 7, 14), c(0, 1, 2, 3, 7, 14))
  names(lags_init_na) <- "death_rate"

  expect_equal(arx_lags_validator(pred_vec, lags_init_na), lags_init_na)

  # Try use a name not in predictors - Error
  lags_init_other_name <- list(death_rate = c(0, 7, 14), test_var = c(0, 1, 2, 3, 7, 14))

  expect_error(arx_lags_validator(pred_vec, lags_init_other_name))
})
