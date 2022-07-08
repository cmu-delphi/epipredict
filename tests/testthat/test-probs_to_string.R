test_that("tests get_precision (i.e. decimal point precision)",{
  expect_equal(get_precision(2),0)
  expect_equal(get_precision(c(3.0,3.12,3.003)),c(0,2,3))
  expect_identical(get_precision(NA),NA)
})

test_that("probs_to_string throws errors when it should",{
  # Must be at most 1 and at least 0
  expect_error(probs_to_string(100))
  expect_error(probs_to_string(-2))
  # Second argument must be of length 1
  expect_error(probs_to_string(0.5,c("a","b")))
})

test_that("probs_to_string works properly",{
  expect_null(probs_to_string(NULL))
  expect_equal(probs_to_string(c(0.2,0.45),"abc"),c("abc0.2","abc0.45"))
})
