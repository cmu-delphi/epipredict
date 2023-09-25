test_that("quantile pivotting wider behaves", {
  tib <- tibble::tibble(a = 1:5, b = 6:10)
  expect_error(pivot_quantiles_wider(tib, a))
  tib$c <- rep(dist_normal(), 5)
  expect_error(pivot_quantiles_wider(tib, c))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:5, 1:4 / 5))
  # different quantiles
  tib <- tib[1:2, ]
  tib$d1 <- d1
  expect_error(pivot_quantiles_wider(tib, d1))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 2:4 / 4))
  tib$d1 <- d1
  # would want to error (mismatched quantiles), but hard to check efficiently
  expect_silent(pivot_quantiles_wider(tib, d1))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 1:3 / 4))
  d2 <- c(dist_quantiles(2:4, 2:4 / 5), dist_quantiles(3:5, 2:4 / 5))
  tib <- tibble::tibble(g = c("a", "b"), d1 = d1, d2 = d2)


  expect_length(pivot_quantiles_wider(tib, c("d1", "d2")), 7L)
  expect_length(pivot_quantiles_wider(tib, tidyselect::starts_with("d")), 7L)
  expect_length(pivot_quantiles_wider(tib, d2), 5L)
})


test_that("quantile pivotting longer behaves", {
  tib <- tibble::tibble(a = 1:5, b = 6:10)
  expect_error(pivot_quantiles_longer(tib, a))
  tib$c <- rep(dist_normal(), 5)
  expect_error(pivot_quantiles_longer(tib, c))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:5, 1:4 / 5))
  # different quantiles
  tib <- tib[1:2, ]
  tib$d1 <- d1
  expect_length(pivot_quantiles_longer(tib, d1), 5L)
  expect_identical(nrow(pivot_quantiles_longer(tib, d1)), 7L)
  expect_identical(pivot_quantiles_longer(tib, d1)$q, as.double(c(1:3, 2:5)))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 2:4 / 4))
  tib$d1 <- d1
  expect_silent(pivot_quantiles_longer(tib, d1))

  d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 1:3 / 4))
  d2 <- c(dist_quantiles(2:4, 2:4 / 5), dist_quantiles(3:5, 2:4 / 5))
  tib <- tibble::tibble(g = c("a", "b"), d1 = d1, d2 = d2)


  expect_length(pivot_quantiles_longer(tib, c("d1", "d2")), 5L)
  expect_identical(nrow(pivot_quantiles_longer(tib, c("d1", "d2"))), 6L)
  expect_silent(pivot_quantiles_longer(tib, tidyselect::starts_with("d")))
  expect_length(pivot_quantiles_longer(tib, d2), 5L)

  tib$d3 <- c(dist_quantiles(2:5, 2:5 / 6), dist_quantiles(3:6, 2:5 / 6))
  # now the cols have different numbers of quantiles
  expect_error(pivot_quantiles_longer(tib, d1, d3))
  expect_length(
    pivot_quantiles_longer(tib, d1, d3, .ignore_length_check = TRUE),
    6L
  )
  expect_identical(
    pivot_quantiles_longer(tib, d1, d3, .ignore_length_check = TRUE)$d1_q,
    as.double(rep(c(1:3, 2:4), each = 4))
  )
})
