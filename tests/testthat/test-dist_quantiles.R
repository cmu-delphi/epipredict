library(distributional)

test_that("constructor returns reasonable quantiles", {
  expect_snapshot(error = TRUE, new_quantiles(rnorm(5), c(-2, -1, 0, 1, 2)))
  expect_silent(new_quantiles(sort(rnorm(5)), sort(runif(5))))
  expect_snapshot(error = TRUE, new_quantiles(sort(rnorm(5)), sort(runif(2))))
  expect_silent(new_quantiles(1:5, 1:5 / 10))
  expect_snapshot(error = TRUE, new_quantiles(c(2, 1, 3, 4, 5), c(.1, .1, .2, .5, .8)))
  expect_snapshot(error = TRUE, new_quantiles(c(2, 1, 3, 4, 5), c(.1, .15, .2, .5, .8)))
  expect_snapshot(error = TRUE, new_quantiles(c(1, 2, 3), c(.1, .2, 3)))
})


test_that("single dist_quantiles works, quantiles are accessible", {
  z <- new_quantiles(values = 1:5, quantile_levels = c(.2, .4, .5, .6, .8))
  expect_s3_class(z, "dist_quantiles")
  expect_equal(median(z), 3)
  expect_equal(quantile(z, c(.2, .4, .5, .6, .8)), 1:5)
  expect_equal(quantile(z, c(.3, .7), middle = "linear"), c(1.5, 4.5))

  Q <- stats::splinefun(c(.2, .4, .5, .6, .8), 1:5, method = "hyman")
  expect_equal(quantile(z, c(.3, .7), middle = "cubic"), Q(c(.3, .7)))
  expect_identical(
    extrapolate_quantiles(z, c(.3, .7), middle = "linear"),
    new_quantiles(values = c(1, 1.5, 2, 3, 4, 4.5, 5), quantile_levels = 2:8 / 10)
  )
  # empty values slot results in a length zero distribution
  # see issue #361
  expect_length(dist_quantiles(list(), c(.1, .9)), 0L)
  expect_identical(
    dist_quantiles(list(), c(.1, .9)),
    distributional::dist_degenerate(double())
  )
})


test_that("quantile extrapolator works", {
  dstn <- dist_normal(c(10, 2), c(5, 10))
  qq <- extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
  expect_s3_class(qq, "distribution")
  expect_s3_class(vctrs::vec_data(qq[1])[[1]], "dist_quantiles")
  expect_length(parameters(qq[1])$quantile_levels[[1]], 3L)


  dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
  qq <- extrapolate_quantiles(dstn, probs = c(.25, 0.5, .75))
  expect_s3_class(qq, "distribution")
  expect_s3_class(vctrs::vec_data(qq[1])[[1]], "dist_quantiles")
  expect_length(parameters(qq[1])$quantile_levels[[1]], 7L)

  dstn <- dist_quantiles(1:4, 1:4 / 5)
  qq <- extrapolate_quantiles(dstn, 1:9 / 10)
  dstn_na <- dist_quantiles(c(1, 2, NA, 4), 1:4 / 5)
  qq2 <- extrapolate_quantiles(dstn_na, 1:9 / 10)
  expect_equal(qq, qq2)
  qq3 <- extrapolate_quantiles(dstn_na, 1:9 / 10, replace_na = FALSE)
  qq2_vals <- field(vec_data(qq2)[[1]], "values")
  qq3_vals <- field(vec_data(qq3)[[1]], "values")
  qq2_vals[6] <- NA
  expect_equal(qq2_vals, qq3_vals)
})

test_that("small deviations of quantile requests work", {
  l <- c(.05, .1, .25, .75, .9, .95)
  v <- c(0.0890306, 0.1424997, 0.1971793, 0.2850978, 0.3832912, 0.4240479)
  badl <- l
  badl[1] <- badl[1] - 1e-14
  distn <- dist_quantiles(list(v), list(l))

  # was broken before, now works
  expect_equal(quantile(distn, l), quantile(distn, badl))

  # The tail extrapolation was still poor. It needs to _always_ use
  # the smallest (largest) values or we could end up unsorted
  l <- 1:9 / 10
  v <- 1:9
  distn <- dist_quantiles(list(v), list(l))
  expect_equal(quantile(distn, c(.25, .75)), list(c(2.5, 7.5)))
  expect_equal(quantile(distn, c(.1, .9)), list(c(1, 9)))
  qv <- data.frame(q = l, v = v)
  expect_equal(
    unlist(quantile(distn, c(.01, .05))),
    tail_extrapolate(c(.01, .05), head(qv, 2))
  )
  expect_equal(
    unlist(quantile(distn, c(.99, .95))),
    tail_extrapolate(c(.95, .99), tail(qv, 2))
  )
})

test_that("unary math works on quantiles", {
  dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
  dstn2 <- dist_quantiles(list(log(1:4), log(8:11)), list(c(.2, .4, .6, .8)))
  expect_identical(log(dstn), dstn2)

  dstn2 <- dist_quantiles(list(cumsum(1:4), cumsum(8:11)), list(c(.2, .4, .6, .8)))
  expect_identical(cumsum(dstn), dstn2)
})

test_that("arithmetic works on quantiles", {
  dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
  dstn2 <- dist_quantiles(list(1:4 + 1, 8:11 + 1), list(c(.2, .4, .6, .8)))
  expect_identical(dstn + 1, dstn2)
  expect_identical(1 + dstn, dstn2)

  dstn2 <- dist_quantiles(list(1:4 / 4, 8:11 / 4), list(c(.2, .4, .6, .8)))
  expect_identical(dstn / 4, dstn2)
  expect_identical((1 / 4) * dstn, dstn2)

  expect_snapshot(error = TRUE, sum(dstn))
  expect_snapshot(error = TRUE, suppressWarnings(dstn + distributional::dist_normal()))
})
