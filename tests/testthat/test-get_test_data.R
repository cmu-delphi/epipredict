suppressPackageStartupMessages(library(dplyr))
test_that("return expected number of rows and returned dataset is ungrouped", {
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14, 21, 28)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  test <- get_test_data(recipe = r, x = case_death_rate_subset)

  expect_equal(
    nrow(test),
    dplyr::n_distinct(case_death_rate_subset$geo_value) * 29
  )

  expect_false(dplyr::is.grouped_df(test))
})


test_that("expect insufficient training data error", {
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 367)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  expect_snapshot(error = TRUE, get_test_data(recipe = r, x = case_death_rate_subset))
})


test_that("expect error that geo_value or time_value does not exist", {
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  wrong_epi_df <- case_death_rate_subset %>% dplyr::select(-geo_value)

  expect_snapshot(error = TRUE, get_test_data(recipe = r, x = wrong_epi_df))
})


test_that("NA fill behaves as desired", {
  testthat::skip()
  df <- tibble::tibble(
    geo_value = rep(c("ca", "ny"), each = 10),
    time_value = rep(1:10, times = 2),
    x1 = rnorm(20),
    x2 = rnorm(20)
  ) %>%
    epiprocess::as_epi_df()

  r <- epi_recipe(df) %>%
    step_epi_ahead(x1, ahead = 3) %>%
    step_epi_lag(x1, x2, lag = c(1, 3)) %>%
    step_epi_naomit()

  expect_silent(tt <- get_test_data(r, df))
  expect_s3_class(tt, "epi_df")

  expect_snapshot(error = TRUE, get_test_data(r, df, "A"))
  expect_snapshot(error = TRUE, get_test_data(r, df, TRUE, -3))

  df2 <- df
  df2$x1[df2$geo_value == "ca"] <- NA

  td <- get_test_data(r, df2)
  expect_true(any(is.na(td)))
  expect_snapshot(error = TRUE, get_test_data(r, df2, TRUE))

  df1 <- df2
  df1$x1[1:4] <- 1:4
  td1 <- get_test_data(r, df1, TRUE, n_recent = 7)
  expect_true(!any(is.na(td1)))

  df2$x1[7:8] <- 1:2
  td2 <- get_test_data(r, df2, TRUE)
  expect_true(!any(is.na(td2)))
})

test_that("forecast date behaves", {
  testthat::skip()
  df <- tibble::tibble(
    geo_value = rep(c("ca", "ny"), each = 10),
    time_value = rep(1:10, times = 2),
    x1 = rnorm(20),
    x2 = rnorm(20)
  ) %>%
    epiprocess::as_epi_df()

  r <- epi_recipe(df) %>%
    step_epi_ahead(x1, ahead = 3) %>%
    step_epi_lag(x1, x2, lag = c(1, 3))

  expect_snapshot(error = TRUE, get_test_data(r, df, TRUE, forecast_date = 9)) # class error
  expect_snapshot(error = TRUE, get_test_data(r, df, TRUE, forecast_date = 9L)) # fd too early
  expect_snapshot(error = TRUE, get_test_data(r, df, forecast_date = 9L)) # fd too early

  ndf <- get_test_data(r, df, TRUE, forecast_date = 12L)
  expect_equal(max(ndf$time_value), 11L) # max lag was 1
  expect_equal(tail(ndf$x1, 2), tail(ndf$x1, 4)[1:2]) # should have filled

  ndf <- get_test_data(r, df, FALSE, forecast_date = 12L)
  expect_equal(max(ndf$time_value), 11L)
  expect_equal(tail(ndf$x1, 2), as.double(c(NA, NA)))
})

test_that("Omit end rows according to minimum lag when that’s not lag 0", {
  # Simple toy ex

  toy_epi_df <- tibble::tibble(
    time_value = seq(as.Date("2020-01-01"),
      by = 1,
      length.out = 10
    ),
    geo_value = "ak",
    x = 1:10
  ) %>% epiprocess::as_epi_df()

  toy_rec <- epi_recipe(toy_epi_df) %>%
    step_epi_lag(x, lag = c(2, 4)) %>%
    step_epi_ahead(x, ahead = 3) %>%
    step_epi_naomit()

  toy_td <- get_test_data(toy_rec, toy_epi_df)

  toy_td_res <- bake(prep(toy_rec, toy_epi_df), toy_td)

  expect_equal(ncol(toy_td_res), 6L)
  expect_equal(nrow(toy_td_res), 1L)
  expect_equal(toy_td_res$time_value, as.Date("2020-01-10"))
  expect_equal(toy_epi_df[toy_epi_df$time_value == as.Date("2020-01-08"), ]$x, toy_td_res$lag_2_x)
  expect_equal(toy_epi_df[toy_epi_df$time_value == as.Date("2020-01-06"), ]$x, toy_td_res$lag_4_x)
  expect_equal(toy_td_res$x, NA_integer_)
  expect_equal(toy_td_res$ahead_3_x, NA_integer_)

  # Ex. using real built-in data

  ca <- case_death_rate_subset %>%
    filter(geo_value == "ca")

  rec <- epi_recipe(ca) %>%
    step_epi_lag(case_rate, lag = c(2, 4, 6)) %>%
    step_epi_ahead(case_rate, ahead = 7) %>%
    step_epi_naomit()

  td <- get_test_data(rec, ca)

  td_res <- bake(prep(rec, ca), td)
  td_row1to5_res <- bake(prep(rec, ca), td[1:5, ])

  expect_equal(td_res, td_row1to5_res)
  expect_equal(nrow(td_res), 1L)
  expect_equal(td_res$time_value, as.Date("2021-12-31"))
  expect_equal(ca[ca$time_value == as.Date("2021-12-29"), ]$case_rate, td_res$lag_2_case_rate)
  expect_equal(ca[ca$time_value == as.Date("2021-12-27"), ]$case_rate, td_res$lag_4_case_rate)
  expect_equal(ca[ca$time_value == as.Date("2021-12-25"), ]$case_rate, td_res$lag_6_case_rate)
})
