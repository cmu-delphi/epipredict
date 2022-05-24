df <- data.frame(matrix(1:100, ncol = 5))
mat <- matrix(1:4, ncol = 2)
df_mat_mul(df, mat, "z", dplyr::num_range("X", 2:3))
test_that("df_mat_mul checks inputs", {
  expect_error(df_mat_mul(30,mat))
  expect_error(df_mat_mul(df,20))
})

test_that("Incompatible matrix multipication cannot happen", {
  expect_error(df_mat_mul(df, mat, "z", dplyr::num_range("X", 1:3)))
})

test_that("Matrix multiplication is being handled as expected", {
  X <- df[c(1,4,5)]
  Z <- as.data.frame(as.matrix(df[2:3]) %*% mat)
  colnames(Z) <- c("z1","z2")
  output <- cbind(X,Z)
  expect_identical(df_mat_mul(df,mat, "z", dplyr::num_range("X", 2:3)),output)
})
