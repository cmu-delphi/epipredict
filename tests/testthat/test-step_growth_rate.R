test_that("step_growth_rate validates arguments", {
  df <- data.frame(time_value = 1:5, geo_value = rep("a", 5), value = 6:10)
  r <- recipes::recipe(df)
  expect_error(step_growth_rate(r))

  edf <- as_epi_df(df)
  r <- epi_recipe(edf)

  expect_error(step_growth_rate(r, value, role = 1))
  expect_error(step_growth_rate(r, value, method = "abc"))
  expect_error(step_growth_rate(r, value, horizon = 0))
  expect_error(step_growth_rate(r, value, horizon = c(1, 2)))
  expect_error(step_growth_rate(r, value, prefix = letters[1:2]))
  expect_error(step_growth_rate(r, value, id = letters[1:2]))
  expect_error(step_growth_rate(r, value, prefix = letters[1:2]))
  expect_error(step_growth_rate(r, value, prefix = 1))
  expect_error(step_growth_rate(r, value, id = 1))
  expect_error(step_growth_rate(r, value, trained = 1))
  expect_error(step_growth_rate(r, value, log_scale = 1))
  expect_error(step_growth_rate(r, value, skip = 1))
  expect_error(step_growth_rate(r, value, additional_gr_args_list = 1:5))
  expect_error(step_growth_rate(r, value, columns = letters[1:5]))
  expect_error(step_growth_rate(r, value, replace_Inf = "c"))
  expect_error(step_growth_rate(r, value, replace_Inf = c(1, 2)))
  expect_silent(step_growth_rate(r, value, replace_Inf = NULL))
  expect_silent(step_growth_rate(r, value, replace_Inf = NA))

})


test_that("step_growth_rate works for a single signal", {
  df <- data.frame(time_value = 1:5, geo_value = rep("a", 5), value = 6:10)
  edf <- as_epi_df(df)
  r <- epi_recipe(edf)

  res <- r %>% step_growth_rate(value, horizon = 1) %>% prep() %>% bake(edf)
  expect_equal(res$gr_1_rel_change_value, c(NA, 1 / 6:9))

  df <- dplyr::bind_rows(
    df,
    data.frame(time_value = 1:5, geo_value = rep("b", 5), value = 6:10)
  )
  edf <- as_epi_df(df)
  r <- epi_recipe(edf)
  res <- r %>% step_growth_rate(value, horizon = 1) %>% prep() %>% bake(edf)
  expect_equal(res$gr_1_rel_change_value, rep(c(NA, 1 / 6:9), each = 2))

})


test_that("step_growth_rate works for a two signals", {
  df <- data.frame(time_value = 1:5,
                   geo_value = rep("a", 5),
                   v1 = 6:10, v2 = 1:5)
  edf <- as_epi_df(df)
  r <- epi_recipe(edf)

  res <- r %>% step_growth_rate(v1, v2, horizon = 1) %>% prep() %>% bake(edf)
  expect_equal(res$gr_1_rel_change_v1, c(NA, 1 / 6:9))
  expect_equal(res$gr_1_rel_change_v2, c(NA, 1 / 1:4))

  df <- dplyr::bind_rows(
    df,
    data.frame(time_value = 1:5, geo_value = rep("b", 5), v1 = 6:10, v2 = 1:5)
  )
  edf <- as_epi_df(df)
  r <- epi_recipe(edf)
  res <- r %>% step_growth_rate(v1, v2, horizon = 1) %>% prep() %>% bake(edf)
  expect_equal(res$gr_1_rel_change_v1, rep(c(NA, 1 / 6:9), each = 2))
  expect_equal(res$gr_1_rel_change_v2, rep(c(NA, 1 / 1:4), each = 2))

})
