test_that("arx returns proper empty tibble", {
  template1 <- tibble::tibble(key_vars = 1:10, point = NA)
  template1 <- enframer(template1, c("q0.05", "q0.95"))
  expect_identical(
    arx_forecaster(1:100, 1:10, key_vars = 1:10, 1:10),
    template1
  )
  names(template1)[1] = "aaa"
  expect_identical(
    arx_forecaster(1:100, 1:10, data.frame(aaa = 1:10), 1:10),
    template1
  )
})

test_that("simple forms produce output", {
  x <- rnorm(100)
  y <- rnorm(100)
  out1 <- arx_forecaster(x, y, NULL, 1:100)
  expect_equal(nrow(out1), 1L)
  expect_named(out1, c("point", "q0.05", "q0.95"))

  out2 <- arx_forecaster(x, y, rep(letters[1:2], each=50), rep(1:50, times=2))
  expect_equal(nrow(out2), 2L)
  expect_named(out2, c("key_vars", "point", "q0.05", "q0.95"))

  out3 <- arx_forecaster(x, y,
                         data.frame(geo_value = rep(letters[1:2], each=50)),
                         rep(1:50, times=2),
                         arx_args_list(levels = c(.5, .8, .9)))
  expect_equal(nrow(out3), 2L)
  expect_named(out3, c("geo_value", "point", "q0.5", "q0.8", "q0.9"))
})
