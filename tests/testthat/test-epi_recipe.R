

test_that("epi_recipe produces default recipe", {
  # these all call recipes::recipe(), but the template will always have 1 row
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5)
  )
  rec <- recipes::recipe(tib)
  rec$template <- rec$template[1,]
  expect_identical(rec, epi_recipe(tib))

  rec <- recipes::recipe(y~x, tib)
  rec$template <- rec$template[1,]
  expect_identical(rec, epi_recipe(y ~ x, tib))

  m <- as.matrix(tib)
  rec <- recipes::recipe(m)
  rec$template <- rec$template[1,]
  expect_identical(rec, epi_recipe(m))

})

test_that("epi_recipe formula works", {
  # these all call recipes::recipe(), but the template will always have 1 row
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
    geo_value = "ca"
  ) %>% epiprocess::as_epi_df()

  epi_recipe(y~x, tib)
})



