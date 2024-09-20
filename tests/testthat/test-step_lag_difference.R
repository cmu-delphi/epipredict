test_that("step_lag_difference validates arguments", {
  df <- data.frame(time_value = 1:5, geo_value = rep("a", 5), value = 6:10)
  r <- recipes::recipe(df)
  expect_error(step_lag_difference(r))

  edf <- as_epi_df(df)
  r <- recipe(edf)

  expect_error(step_lag_difference(r, value, role = 1))
  expect_error(step_lag_difference(r, value, horizon = 0))
  expect_silent(step_lag_difference(r, value, horizon = c(1, 2)))
  expect_error(step_lag_difference(r, value, prefix = letters[1:2]))
  expect_error(step_lag_difference(r, value, id = letters[1:2]))
  expect_error(step_lag_difference(r, value, prefix = letters[1:2]))
  expect_error(step_lag_difference(r, value, prefix = 1))
  expect_error(step_lag_difference(r, value, id = 1))
  expect_error(step_lag_difference(r, value, skip = 1))
})


test_that("step_lag_difference works for a single signal", {
  df <- data.frame(time_value = 1:5, geo_value = rep("a", 5), value = 6:10)
  edf <- as_epi_df(df)
  r <- recipe(edf)

  res <- r %>%
    step_lag_difference(value, horizon = 1) %>%
    prep(edf) %>%
    bake(edf)
  expect_equal(res$lag_diff_1_value, c(NA, rep(1, 4)))

  res <- r %>%
    step_lag_difference(value, horizon = 1:2) %>%
    prep(edf) %>%
    bake(edf)
  expect_equal(res$lag_diff_1_value, c(NA, rep(1, 4)))
  expect_equal(res$lag_diff_2_value, c(NA, NA, rep(2, 3)))



  df <- dplyr::bind_rows(
    df,
    data.frame(time_value = 1:5, geo_value = rep("b", 5), value = 6:10)
  )
  edf <- as_epi_df(df)
  r <- recipe(edf)
  res <- r %>%
    step_lag_difference(value, horizon = 1) %>%
    prep(edf) %>%
    bake(edf)
  expect_equal(res$lag_diff_1_value, c(NA, NA, rep(1, 8)))
})


test_that("step_lag_difference works for a two signals", {
  df <- data.frame(
    time_value = 1:5,
    geo_value = rep("a", 5),
    v1 = 6:10, v2 = 1:5 * 2
  )
  edf <- as_epi_df(df)
  r <- recipe(edf)

  res <- r %>%
    step_lag_difference(v1, v2, horizon = 1:2) %>%
    prep(edf) %>%
    bake(edf)
  expect_equal(res$lag_diff_1_v1, c(NA, rep(1, 4)))
  expect_equal(res$lag_diff_2_v1, c(NA, NA, rep(2, 3)))
  expect_equal(res$lag_diff_1_v2, c(NA, rep(2, 4)))
  expect_equal(res$lag_diff_2_v2, c(NA, NA, rep(4, 3)))

  df <- dplyr::bind_rows(
    df,
    data.frame(time_value = 1:5, geo_value = rep("b", 5), v1 = 6:10, v2 = 1:5)
  )
  edf <- as_epi_df(df)
  r <- recipe(edf)
  res <- r %>%
    step_lag_difference(v1, v2, horizon = 1:2) %>%
    prep(edf) %>%
    bake(edf)
  expect_equal(res$lag_diff_1_v1, rep(c(NA, rep(1, 4)), each = 2))
  expect_equal(res$lag_diff_2_v1, rep(c(NA, NA, rep(2, 3)), each = 2))
  expect_equal(res$lag_diff_1_v2, c(NA, NA, rep(2:1, 4)))
  expect_equal(res$lag_diff_2_v2, c(rep(NA, 4), rep(c(4, 2), 3)))
})
