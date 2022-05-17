

test_that("epi_recipe produces default recipe", {
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5)
  )
  expect_identical(recipes::recipe(tib), epi_recipe(tib))
  expect_identical(recipes::recipe(y ~ x, tib),
                   epi_recipe(y ~ x, tib))
})
