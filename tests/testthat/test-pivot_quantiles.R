test_that("quantile pivotting behaves", {
  tib <- tibble::tibble(a = 1:5, b = 6:10)
  expect_error(pivot_quantiles(tib, a))
  tib$c <- rep(dist_normal(), 5)
  expect_error(pivot_quantiles(tib, c))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:5, 1:4 / 5))
  # different quantiles
  tib <- tib[1:2,]
  tib$d1 <- d1
  expect_error(pivot_quantiles(tib, d1))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 2:4 / 4))
  tib$d1 <- d1
  # would want to error (mismatched quantiles), but hard to check efficiently
  expect_silent(pivot_quantiles(tib, d1))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 1:3 / 4))
  d2 <- c(dist_quantiles(2:4, 2:4 / 5), dist_quantiles(3:5, 2:4 / 5))
  tib <- tibble::tibble(g = c("a", "b"), d1 = d1, d2 = d2)


  expect_length(pivot_quantiles(tib, c("d1", "d2")), 7L)
  expect_length(pivot_quantiles(tib, tidyselect::starts_with("d")), 7L)
  expect_length(pivot_quantiles(tib, d2), 5L)
})
