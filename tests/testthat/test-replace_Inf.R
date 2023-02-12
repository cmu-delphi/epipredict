test_that("replace_inf works", {
  x <- 1:5
  x[3] <- Inf # converts to double
  expect_identical(vec_replace_inf(x, 3), as.double(1:5))
  df <- tibble(
    geo_value = letters[1:5], time_value = 1:5,
    v1 = 1:5, v2 = c(1,2,Inf, -Inf,NA)
  )

  library(dplyr)
  ok <- c("geo_value", "time_value")
  df2 <- df %>% mutate(across(!all_of(ok), ~ vec_replace_inf(.x, NA)))
  expect_identical(df[,1:3], df2[,1:3])
  expect_identical(df2$v2, c(1,2,NA,NA,NA))
})
