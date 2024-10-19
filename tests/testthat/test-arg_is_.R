a <- "a"
b <- "b"
n <- NULL
nn <- NA
nnn <- c(NA, NA)
i <- 1L
j <- 2L
k <- c(-1L, 1L)
x <- 1.0
y <- 2.0
f <- function(x) x
g <- function(x) x + 3.0
d <- Sys.Date()
dd <- Sys.Date() - 5
v <- 1:5
l <- TRUE
ll <- c(TRUE, FALSE)
z <- character(0)

test_that("logical", {
  expect_silent(arg_is_lgl(l))
  expect_silent(arg_is_lgl(ll))
  expect_silent(arg_is_lgl(l, ll))
  expect_snapshot(error = TRUE, arg_is_lgl(l, ll, n))
  expect_snapshot(error = TRUE, arg_is_lgl(x))
  expect_silent(arg_is_lgl(l, ll, n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_lgl(l, ll, nn))
  expect_silent(arg_is_lgl(l, ll, nn, allow_na = TRUE))
})

test_that("scalar", {
  expect_silent(arg_is_scalar(x))
  expect_silent(arg_is_scalar(dd))
  expect_silent(arg_is_scalar(x, y, dd))
  expect_snapshot(error = TRUE, arg_is_scalar(x, y, n))
  expect_silent(arg_is_scalar(x, y, n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_scalar(x, y, nn))
  expect_silent(arg_is_scalar(x, y, nn, allow_na = TRUE))
  expect_snapshot(error = TRUE, arg_is_scalar(v, nn))
  expect_snapshot(error = TRUE, arg_is_scalar(v, nn, allow_na = TRUE))
  expect_snapshot(error = TRUE, arg_is_scalar(v, n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_scalar(nnn, allow_na = TRUE))
})

test_that("numeric", {
  expect_silent(arg_is_numeric(i, j, x, y))
  expect_snapshot(error = TRUE, arg_is_numeric(a))
  expect_silent(arg_is_numeric(d))
  expect_silent(arg_is_numeric(c(i, j)))
  expect_silent(arg_is_numeric(i, k))
  expect_silent(arg_is_numeric(i, j, n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_numeric(i, j, n))
  expect_snapshot(error = TRUE, arg_is_numeric(i, nn))
  expect_silent(arg_is_numeric(a = -10:10))
})

test_that("positive", {
  expect_silent(arg_is_pos(i, j, x, y))
  expect_snapshot(error = TRUE, arg_is_pos(a))
  expect_silent(arg_is_pos(d))
  expect_silent(arg_is_pos(c(i, j)))
  expect_snapshot(error = TRUE, arg_is_pos(i, k))
  expect_silent(arg_is_pos(i, j, n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_pos(i, j, n))
  expect_snapshot(error = TRUE, arg_is_pos(i, nn))
  expect_snapshot(error = TRUE, arg_is_pos(a = 0:10))
})

test_that("nonneg", {
  expect_silent(arg_is_nonneg(i, j, x, y))
  expect_snapshot(error = TRUE, arg_is_nonneg(a))
  expect_silent(arg_is_nonneg(d))
  expect_silent(arg_is_nonneg(c(i, j)))
  expect_snapshot(error = TRUE, arg_is_nonneg(i, k))
  expect_silent(arg_is_nonneg(i, j, n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_nonneg(i, j, n))
  expect_snapshot(error = TRUE, arg_is_nonneg(i, nn))
  expect_silent(arg_is_nonneg(a = 0:10))
})


test_that("nonneg-int", {
  expect_snapshot(error = TRUE, arg_is_nonneg_int(a))
  expect_snapshot(error = TRUE, arg_is_nonneg_int(d))
  expect_silent(arg_is_nonneg_int(i, j))
  expect_silent(arg_is_nonneg_int(c(i, j)))
  expect_snapshot(error = TRUE, arg_is_nonneg_int(i, k))
  expect_silent(arg_is_nonneg_int(i, j, n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_nonneg_int(i, j, n))
  expect_snapshot(error = TRUE, arg_is_nonneg_int(i, nn))
  expect_silent(arg_is_nonneg_int(a = 0:10))
})

test_that("date", {
  expect_silent(arg_is_date(d, dd))
  expect_silent(arg_is_date(c(d, dd)))
  expect_snapshot(error = TRUE, arg_is_date(d, dd, n))
  expect_snapshot(error = TRUE, arg_is_date(d, dd, nn))
  expect_silent(arg_is_date(d, dd, n, allow_null = TRUE))
  # Upstream issue, see: https://github.com/mllg/checkmate/issues/256
  # expect_silent(arg_is_date(d, dd, nn, allow_na = TRUE))
  expect_snapshot(error = TRUE, arg_is_date(a))
  expect_snapshot(error = TRUE, arg_is_date(v))
  expect_snapshot(error = TRUE, arg_is_date(ll))
})

test_that("probabilities", {
  expect_silent(arg_is_probabilities(i, x))
  expect_snapshot(error = TRUE, arg_is_probabilities(a))
  expect_snapshot(error = TRUE, arg_is_probabilities(d))
  expect_silent(arg_is_probabilities(c(.4, .7)))
  expect_snapshot(error = TRUE, arg_is_probabilities(i, 1.1))
  expect_silent(arg_is_probabilities(c(.4, .8), n, allow_null = TRUE))
  expect_snapshot(error = TRUE, arg_is_probabilities(c(.4, .8), n))
  expect_snapshot(error = TRUE, arg_is_probabilities(c(.4, .8), nn))
})


test_that("chr", {
  expect_silent(arg_is_chr(a, b))
  expect_silent(arg_is_chr(c(a, b)))
  expect_snapshot(error = TRUE, arg_is_chr(a, b, n))
  expect_snapshot(error = TRUE, arg_is_chr(a, b, nn))
  expect_silent(arg_is_chr(a, b, n, allow_null = TRUE))
  expect_silent(arg_is_chr(a, b, nn, allow_na = TRUE))
  expect_snapshot(error = TRUE, arg_is_chr(d))
  expect_snapshot(error = TRUE, arg_is_chr(v))
  expect_snapshot(error = TRUE, arg_is_chr(ll))
  expect_snapshot(error = TRUE, arg_is_chr(z))
  expect_silent(arg_is_chr(z, allow_empty = TRUE))
})

test_that("function", {
  expect_silent(arg_is_function(f, g, parsnip::linear_reg))
  expect_snapshot(error = TRUE, arg_is_function(c(a, b)))
  expect_snapshot(error = TRUE, arg_is_function(c(f, g)))
  f <- NULL
  expect_snapshot(error = TRUE, arg_is_function(f))
  expect_silent(arg_is_function(g, f, allow_null = TRUE))
})

test_that("coerce scalar to date", {
  expect_snapshot(error = TRUE, arg_to_date("12345"))
  expect_s3_class(arg_to_date(12345), "Date")
  expect_s3_class(arg_to_date("2020-01-01"), "Date")
  expect_snapshot(error = TRUE, arg_to_date(c("12345", "12345")))
})

test_that("simple surface step test", {
  expect_snapshot(
    error = TRUE,
    epi_recipe(case_death_rate_subset) %>%
      step_epi_lag(death_rate, lag = "hello")
  )
})
