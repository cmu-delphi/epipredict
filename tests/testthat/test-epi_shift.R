x <- data.frame(x1 = 1:10, x2 = -10:-1)
lags <- list(c(0,4), 1:3)

test_that("epi shift works with NULL keys", {
  time_value <- 1:10
  out <- epi_shift(x, lags, time_value)
  expect_length(out, 7L)
  expect_equal(nrow(out), 14L)
  expect_equal(sum(complete.cases(out)), 6L)
})

test_that("epi shift works with groups", {
  keys <- data.frame(a = rep(letters[1:2], each=5), b = "z")
  time_value <- 1:10
  out <- epi_shift(x, lags, time_value, keys)
  expect_length(out, 8L)
  expect_equal(nrow(out), 18L)
  expect_equal(sum(complete.cases(out)), 2L)
})

test_that("epi shift single works, renames", {
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5)
  ) %>% epiprocess::as_epi_df()
  ess <- epi_shift_single(tib, "x", 1, "test", epi_keys(tib))
  expect_named(ess, c("time_value", "geo_value", "test"))
  expect_equal(ess$time_value, tib$time_value + 1)

})
