test_that("Data",{
  expect_equal(nrow(case_death_rate_subset),20496)
  expect_equal(ncol(case_death_rate_subset),4)
  expect_equal(as.Date("2020-12-31"),min(case_death_rate_subset$time_value))
  expect_equal(as.Date("2021-12-31"),max(case_death_rate_subset$time_value))
})

