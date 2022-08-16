y <- 1:20
x <- -20:-1
time_value <- 1:20
test_that("processing works", {
  lags <- c(1,2,4)
  dat <- create_lags_and_leads(x, y, lags, 1, time_value)
  expect_length(dat, 9)
  expect_equal(nrow(dat), 25)

})

test_that("accepts 1 lag", {
  dat <- create_lags_and_leads(x, y, 1, 1, time_value)
  expect_length(dat, 5)
  expect_identical(nrow(dat), 22L)
})

test_that("dies from incorrect lags",{
  expect_error(create_lags_and_leads(x, y, list(1, 1, 1), 1, time_value))
})

test_that("accepts lag list", {
  lags <- list(c(1,2), c(1))
  dat <- create_lags_and_leads(x, y, lags, 1, time_value)
  expect_length(dat, 6)
  expect_identical(nrow(dat), 23L)

  lags <- list(c(1,2), NULL) # no y lags
  dat <- create_lags_and_leads(x, y, lags, 1, time_value)
  expect_length(dat, 5L)
  expect_identical(nrow(dat), 23L)
})

test_that("accepts leads", {

  lags <- list(c(1,2), c(1))
  ahead <- c(1,2)
  dat <- create_lags_and_leads(x, y, lags, ahead, time_value)
  expect_length(dat, 7)
  expect_identical(nrow(dat), 24L)

  lags <- list(c(1,2), NULL) # no y lags
  dat <- create_lags_and_leads(x, y, lags, ahead, time_value)
  expect_length(dat, 6)
  expect_identical(nrow(dat), 24L)
})

test_that("names are `y/x` (clobbers everything)",{
  lags <- list(c(1,2), c(1))
  ahead <- c(1,2)
  dat <- create_lags_and_leads(x, y, lags, ahead, time_value)
  expect_named(dat, c("keys", "time_value", "y1", "y2", "x1", "x2", "x3"))
})
