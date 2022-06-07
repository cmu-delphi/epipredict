df <- data.frame(matrix(1:100, ncol = 5))
mat <- matrix(1:4, ncol = 2)

test_that("First input must be a data frame and second input must be a matrix",
          {
  expect_error(df_mat_mul(30,mat))
  expect_error(df_mat_mul(df,20))
})

test_that("Argument name is a character" ,{
            expect_error(df_mat_mul(df, mat, 100))
})

test_that("The length of names does not differ from the length of the number
          of outputs" ,{
  expect_error(df_mat_mul(df, mat, c("a","b","c"), 2:3))
})

test_that("The number of columns of the first data frame cannot differ from the
          number of rows of the second matrix, hence preventing incompatible
          matrix multiplication", {
            expect_error(df_mat_mul(df, mat, "z", 1:3))
          })

X <- df[c(1,4,5)]
Z <- as.data.frame(as.matrix(df[2:3]) %*% mat)
colnames(Z) <- c("z1","z2")
output <- cbind(X,Z)

test_that("Names are being handled properly", {
  expect_identical(df_mat_mul(df, mat, "z", 2:3),output)
  expect_identical(df_mat_mul(df, mat, c("z1","z2"), 2:3),output)
})

test_that("Other tidyselect functionalities are working", {
  mult <- df_mat_mul(df, mat, "z", dplyr::num_range("X", 2:3))
  expect_identical(mult,output)
  expect_identical(df_mat_mul(df, mat, "z", 2, 3),output)
  # Mismatched names should not work:
  expect_error(df_mat_mul(df, mat, "z", dplyr::num_range("Y", 2:3)))
})
