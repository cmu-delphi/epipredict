library(distributional)

test_that("constructor returns reasonable quantiles", {
  expect_error(new_quantiles(rnorm(5), rnorm(5)))
  expect_silent(new_quantiles(sort(rnorm(5)), sort(runif(5))))
  expect_error(new_quantiles(sort(rnorm(5)), sort(runif(2))))
  expect_silent(new_quantiles(1:5, 1:5/10))
  expect_error(new_quantiles(c(2,1,3,4,5), c(.1,.1,.2,.5,.8)))
  expect_error(new_quantiles(c(2,1,3,4,5), c(.1,.15,.2,.5,.8)))
  expect_error(new_quantiles(c(1,2,3), c(.1, .2, 3)))
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
  z <- new_quantiles(q = 1:5, tau = c(.2, .4, .5, .6, .8))
  expect_s3_class(z, "dist_quantiles")
  expect_equal(median(z), 3)
  expect_equal(quantile(z, c(.2, .4, .5, .6, .8)), 1:5)
  expect_equal(quantile(z, c(.3, .7), middle = "linear"), c(1.5, 4.5))

  Q <- stats::splinefun(c(.2, .4, .5, .6, .8), 1:5, method = "hyman")
  expect_equal(quantile(z, c(.3, .7), middle = "cubic"), Q(c(.3, .7)))
  expect_identical(
    extrapolate_quantiles(z, c(.3, .7), middle = "linear"),
    new_quantiles(q = c(1,1.5,2,3,4,4.5,5), tau = 2:8/10))
})

test_that("quantile extrapolator works", {

  dstn <- dist_normal(c(10, 2), c(5, 10))
  qq <- extrapolate_quantiles(dstn, p = c(.25, 0.5, .75))
  expect_s3_class(qq, "distribution")
  expect_s3_class(vctrs::vec_data(qq[1])[[1]], "dist_quantiles")
  expect_length(parameters(qq[1])$q[[1]], 3L)


  dstn <- dist_quantiles(list(1:4, 8:11), list(c(.2,.4,.6,.8)))
  qq <- extrapolate_quantiles(dstn, p = c(.25, 0.5, .75))
  expect_s3_class(qq, "distribution")
  expect_s3_class(vctrs::vec_data(qq[1])[[1]], "dist_quantiles")
  expect_length(parameters(qq[1])$q[[1]], 7L)
})
