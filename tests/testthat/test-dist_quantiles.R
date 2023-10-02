library(distributional)

test_that("constructor returns reasonable quantiles", {
  expect_error(new_quantiles(rnorm(5), rnorm(5)))
  expect_silent(new_quantiles(sort(rnorm(5)), sort(runif(5))))
  expect_error(new_quantiles(sort(rnorm(5)), sort(runif(2))))
  expect_silent(new_quantiles(1:5, 1:5 / 10))
  expect_error(new_quantiles(c(2, 1, 3, 4, 5), c(.1, .1, .2, .5, .8)))
  expect_error(new_quantiles(c(2, 1, 3, 4, 5), c(.1, .15, .2, .5, .8)))
  expect_error(new_quantiles(c(1, 2, 3), c(.1, .2, 3)))
})

test_that("tail functions give reasonable output", {
  expect_equal(norm_q_par(qnorm(c(.75, .5), 10, 5)), list(m = 10, s = 5))
  expect_equal(norm_q_par(qnorm(c(.25, .5), 10, 5)), list(m = 10, s = 5))
  expect_equal(norm_q_par(qnorm(c(.25, .5), 0, 1)), list(m = 0, s = 1))
  expect_equal(exp_q_par(qlaplace(c(.75, .5), 10, 5)), list(m = 10, s = 5))
  expect_equal(exp_q_par(qlaplace(c(.25, .5), 10, 5)), list(m = 10, s = 5))
  expect_equal(exp_q_par(qlaplace(c(.25, .5), 0, 1)), list(m = 0, s = 1))
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
})

test_that("quantile extrapolator works", {
  dstn <- dist_normal(c(10, 2), c(5, 10))
  qq <- extrapolate_quantiles(dstn, p = c(.25, 0.5, .75))
  expect_s3_class(qq, "distribution")
  expect_s3_class(vctrs::vec_data(qq[1])[[1]], "dist_quantiles")
  expect_length(parameters(qq[1])$q[[1]], 3L)


  dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2, .4, .6, .8)))
  qq <- extrapolate_quantiles(dstn, p = c(.25, 0.5, .75))
  expect_s3_class(qq, "distribution")
  expect_s3_class(vctrs::vec_data(qq[1])[[1]], "dist_quantiles")
  expect_length(parameters(qq[1])$q[[1]], 7L)
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

  expect_error(sum(dstn))
  expect_error(suppressWarnings(dstn + distributional::dist_normal()))
})
