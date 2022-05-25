test_that("First argument must be a list",{
  expect_error(assign_arg_list(c(1,2,3)))
})
test_that("All arguments should be named",{
  expect_error(assign_arg_list(list(1,2)))
})
