test_that("enframer errors/works as needed", {
  template1 <- data.frame(aa = 1:5, a=NA, b=NA, c=NA)
  template2 <- data.frame(aa = 1:5, a=2:6, b=2:6, c=2:6)
  expect_error(enframer(1:5, letters[1]))
  expect_error(enframer(data.frame(a = 1:5), 1:3))
  expect_error(enframer(data.frame(a = 1:5), letters[1:3]))
  expect_identical(enframer(data.frame(aa = 1:5), letters[1:3]), template1)
  expect_error(enframer(data.frame(aa = 1:5), letters[1:2], fill=1:4))
  expect_identical(
    enframer(data.frame(aa = 1:5), letters[1:3], fill=2:6),
    template2)
})
