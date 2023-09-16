df <- data.frame(b = 1, c = 2, ca = 3, cat = 4)

test_that("Names are grabbed properly", {
  expect_identical(
    grab_names(df, dplyr::starts_with("ca")),
    subset(names(df), startsWith(names(df), "ca"))
  )
})
