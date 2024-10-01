test_that("wis dispatches and produces the correct values", {
  tau <- c(.2, .4, .6, .8)
  q1 <- 1:4
  q2 <- 8:11
  wis_one_pred <- function(q, tau, actual) {
    2 * mean(pmax(tau * (actual - q), (1 - tau) * (q - actual)), na.rm = TRUE)
  }
  actual <- 5
  expected <- c(wis_one_pred(q1, tau, actual), wis_one_pred(q2, tau, actual))

  dstn <- quantile_pred(rbind(q1, q2), tau)
  expect_equal(weighted_interval_score(dstn, actual), expected)

  # works with a single dstn
  q <- sort(10 * rexp(23))
  tau0 <- c(.01, .025, 1:19 / 20, .975, .99)
  dst <- quantile_pred(rbind(q), tau0)
  expect_equal(weighted_interval_score(dst, 10), wis_one_pred(q, tau0, 10))

  # returns NA when expected
  dst <- quantile_pred(rbind(rep(NA, 3)), c(.2, .5, .95))
  expect_true(is.na(weighted_interval_score(dst, 10)))
  expect_equal(
    weighted_interval_score(dstn, c(NA, actual)),
    c(NA, wis_one_pred(q2, tau, actual))
  )

  # errors for non quantile_pred
  expect_snapshot(error = TRUE, weighted_interval_score(1:10, 10))

  # errors if sizes don't match
  expect_snapshot(error = TRUE, weighted_interval_score(
    quantile_pred(rbind(1:4, 8:11), 1:4 / 5), # length 2
    1:3
  ))

  #' # Missing value behaviours
  dstn <- quantile_pred(rbind(c(1, 2, NA, 4)), 1:4 / 5)
  expect_equal(weighted_interval_score(dstn, 2.5), 0.5)
  expect_equal(weighted_interval_score(dstn, 2.5, c(2, 4, 5, 6, 8) / 10), 0.4)
  expect_equal(
    weighted_interval_score(dstn, 3, na_handling = "drop"),
    2 / 3
  )
  expect_equal(
    weighted_interval_score(dstn, 2.5, c(2, 4, 5, 6, 8) / 10, na_handling = "drop"),
    0.4
  )
  expect_true(is.na(
    weighted_interval_score(dstn, 2.5, na_handling = "propagate")
  ))
  expect_true(is.na(
    weighted_interval_score(
      quantile_pred(rbind(1:4), 1:4 / 5), 2.5, 1:9 / 10, na_handling = "fail"
    )
  ))
})
