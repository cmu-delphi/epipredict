y <- 1:20
x <- -20:-1

test_that("processing works", {
  lags <- c(1,2,4)
  dat <- create_lags_and_leads(x, y, lags, 1)
  expect_length(dat, 7)
  expect_equal(nrow(dat), 20)
})

test_that("accepts 1 lag", {
  dat <- create_lags_and_leads(x, y, 1, 1)
  expect_length(dat, 3)
  expect_identical(nrow(dat), 20L)
})

test_that("dies from incorrect lags",{
  expect_error(create_lags_and_leads(x, y, list(1, 1, 1), 1))
})

test_that("accepts lag list", {
  lags <- list(c(1,2), c(1))
  dat <- create_lags_and_leads(x, y, lags, 1)
  expect_length(dat, 4)
  expect_identical(nrow(dat), 20L)

  lags <- list(c(1,2), NULL) # no y lags
  dat <- create_lags_and_leads(x, y, lags, 1)
  expect_length(dat, 3)
  expect_identical(nrow(dat), 20L)
})

test_that("accepts leads", {

  lags <- list(c(1,2), c(1))
  ahead <- c(1,2)
  dat <- create_lags_and_leads(x, y, lags, ahead)
  expect_length(dat, 5)
  expect_identical(nrow(dat), 20L)

  lags <- list(c(1,2), NULL) # no y lags
  dat <- create_lags_and_leads(x, y, lags, ahead)
  expect_length(dat, 4)
  expect_identical(nrow(dat), 20L)
})

test_that("names are `y/x` (clobbers everything)",{
  lags <- list(c(1,2), c(1))
  ahead <- c(1,2)
  dat <- create_lags_and_leads(x, y, lags, ahead)
  expect_named(dat, c("y1", "y2", "x1", "x2", "x3"))
})
