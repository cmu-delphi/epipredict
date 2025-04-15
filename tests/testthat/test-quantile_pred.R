test_that("single quantile_pred works, quantiles are accessible", {
  z <- hardhat::quantile_pred(
    values = matrix(1:5, nrow = 1),
    quantile_levels = c(.2, .4, .5, .6, .8)
  )
  expect_equal(median(z), 3)
  expect_equal(quantile(z, c(.2, .4, .5, .6, .8)), matrix(1:5, nrow = 1))
  expect_equal(
    quantile(z, c(.3, .7), middle = "linear"),
    matrix(c(1.5, 4.5), nrow = 1)
  )

  Q <- stats::splinefun(c(.2, .4, .5, .6, .8), 1:5, method = "hyman")
  expect_equal(quantile(z, c(.3, .7)), matrix(Q(c(.3, .7)), nrow = 1))
  expect_identical(
    extrapolate_quantiles(z, c(.3, .7), middle = "linear"),
    hardhat::quantile_pred(matrix(c(1, 1.5, 2, 3, 4, 4.5, 5), nrow = 1), 2:8 / 10)
  )
})


test_that("quantile extrapolator works", {
  dstn <- hardhat::quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE),
    c(.2, .4, .6, .8)
  )
  qq <- extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
  expect_s3_class(qq, c("quantile_pred", "vctrs_vctr", "list"))
  expect_length(qq %@% "quantile_levels", 7L)

  dstn <- hardhat::quantile_pred(matrix(1:4, nrow = 1), 1:4 / 5)
  qq <- extrapolate_quantiles(dstn, 1:9 / 10)
  dstn_na <- hardhat::quantile_pred(matrix(c(1, 2, NA, 4), nrow = 1), 1:4 / 5)
  qq2 <- extrapolate_quantiles(dstn_na, 1:9 / 10)
  expect_equal(qq, qq2)
  qq3 <- extrapolate_quantiles(dstn_na, 1:9 / 10, replace_na = FALSE)
  qq2_vals <- unlist(qq2)
  qq3_vals <- unlist(qq3)
  qq2_vals[6] <- NA
  expect_equal(qq2_vals, qq3_vals)
})

test_that("small deviations of quantile requests work", {
  l <- c(.05, .1, .25, .75, .9, .95)
  v <- c(0.0890306, 0.1424997, 0.1971793, 0.2850978, 0.3832912, 0.4240479)
  badl <- l
  badl[1] <- badl[1] - 1e-14
  distn <- hardhat::quantile_pred(matrix(v, nrow = 1), l)

  # was broken before, now works
  expect_equal(quantile(distn, l), quantile(distn, badl))

  # The tail extrapolation was still poor. It needs to _always_ use
  # the smallest (largest) values or we could end up unsorted
  l <- 1:9 / 10
  v <- 1:9
  distn <- hardhat::quantile_pred(matrix(v, nrow = 1), l)
  expect_equal(quantile(distn, c(.25, .75)), matrix(c(2.5, 7.5), nrow = 1))
  expect_equal(quantile(distn, c(.1, .9)), matrix(c(1, 9), nrow = 1))
  qv <- data.frame(q = l, v = v)
  expect_equal(
    drop(quantile(distn, c(.01, .05))),
    tail_extrapolate(c(.01, .05), head(qv, 2))
  )
  expect_equal(
    drop(quantile(distn, c(.99, .95))),
    tail_extrapolate(c(.95, .99), tail(qv, 2))
  )
})

test_that("unary math works on quantiles", {
  dstn <- hardhat::quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE),
    1:4 / 5
  )
  dstn2 <- hardhat::quantile_pred(
    log(matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE)),
    1:4 / 5
  )
  expect_identical(log(dstn), dstn2)
})

test_that("arithmetic works on quantiles", {
  # Quantile and numeric arithmetic works
  dstn <- hardhat::quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE),
    1:4 / 5
  )
  dstn2 <- hardhat::quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE) + 1,
    1:4 / 5
  )
  expect_identical(dstn + 1, dstn2)
  expect_identical(1 + dstn, dstn2)

  dstn2 <- hardhat::quantile_pred(
    matrix(c(1:4, 8:11), nrow = 2, byrow = TRUE) / 4,
    1:4 / 5
  )
  expect_identical(dstn / 4, dstn2)
  expect_identical((1 / 4) * dstn, dstn2)

  expect_snapshot(error = TRUE, sum(dstn))

  # Quantile and quantile arithmetic works
  val <- c(1:4, 8:11)
  dstn3 <- hardhat::quantile_pred(
    matrix(val, nrow = 2, byrow = TRUE),
    1:4 / 5
  )
  dstn4 <- hardhat::quantile_pred(
    matrix(val + 2 * val, nrow = 2, byrow = TRUE),
    1:4 / 5
  )
  expect_identical(dstn3 + (2 * dstn3), dstn4)

  # Extrapolate when quantile_levels are not the same
  val <- c(1:4, 8:11)
  dstn5 <- hardhat::quantile_pred(
    matrix(val, nrow = 2, byrow = TRUE),
    c(0.1, 0.25, 0.5, 0.75)
  )
  dstn6 <- hardhat::quantile_pred(
    matrix(val, nrow = 2, byrow = TRUE),
    c(0.25, 0.5, 0.75, 0.9)
  )
  expect_identical((dstn5 + dstn6) %@% "quantile_levels", c(0.1, 0.25, 0.5, 0.75, 0.9))
})
