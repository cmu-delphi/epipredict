test_that("step_growth_rate validates arguments", {
  df <- data.frame(time_value = 1:5, geo_value = rep("a", 5), value = 6:10)
  r <- recipes::recipe(df)
  expect_error(step_growth_rate(r))
  edf <- as_epi_df(df)

})
