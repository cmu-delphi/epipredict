test_that("epi shift single works, renames", {
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
    geo_value = "ca"
  ) %>% epiprocess::as_epi_df()
  ess <- epi_shift_single(tib, "x", 1, "test", epi_keys(tib))
  expect_named(ess, c("time_value", "geo_value", "test"))
  expect_equal(ess$time_value, tib$time_value + 1)
})
