test_that("wis dispatches and produces the correct values", {
  tau <- c(.2, .4, .6, .8)
  q1 <- 1:4
  q2 <- 8:11
  wis_one_pred <- function(q, tau, actual) {
    2 * mean(pmax(tau * (actual - q), (1 - tau) * (q - actual)), na.rm = TRUE)
  }
  actual <- 5
  expected <- c(wis_one_pred(q1, tau, actual), wis_one_pred(q2, tau, actual))

  dstn <- dist_quantiles(list(q1, q2), tau)
  expect_equal(weighted_interval_score(dstn, actual), expected)

  # works with a single dstn
  q <- sort(10 * rexp(23))
  tau0 <- c(.01, .025, 1:19 / 20, .975, .99)
  dst <- dist_quantiles(q, tau0)
  expect_equal(weighted_interval_score(dst, 10), wis_one_pred(q, tau0, 10))

  # returns NA when expected
  dst <- dist_quantiles(rep(NA, 3), c(.2, .5, .95))
  expect_true(is.na(weighted_interval_score(dst, 10)))
  expect_equal(
    weighted_interval_score(dstn, c(NA, actual)),
    c(NA, wis_one_pred(q2, tau, actual))
  )

  # non-NA where possible
  expect_equal(
    weighted_interval_score(dist_quantiles(c(1, 2, NA, 4), 1:4 / 5), 3),
    2 / 3
  )

  # errors for non distributions
  expect_error(weighted_interval_score(1:10, 10))
  expect_warning(w <- weighted_interval_score(dist_normal(1), 10))
  expect_true(all(is.na(w)))
  expect_warning(w <- weighted_interval_score(
    c(dist_normal(), dist_quantiles(1:5, 1:5 / 6)),
    10
  ))
  expect_equal(w, c(NA, wis_one_pred(1:5, 1:5 / 6, 10)))

  # errors if sizes don't match
  expect_error(weighted_interval_score(
    dist_quantiles(list(1:4, 8:11), 1:4 / 5), # length 2
    1:3
  ))
})
