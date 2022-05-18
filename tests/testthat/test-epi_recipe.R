

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

  r <- epi_recipe(y~x, tib)
  ref_var_info <- tibble::tribble(
    ~ variable, ~ type, ~ role, ~ source,
    "x", "numeric", "predictor", "original",
    "y", "numeric", "outcome", "original",
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "key", "original"
  )
  expect_identical(r$var_info, ref_var_info)

  r <- epi_recipe(y ~ x + geo_value, tib)
  ref_var_info <- tibble::tribble(
    ~ variable, ~ type, ~ role, ~ source,
    "x", "numeric", "predictor", "original",
    "geo_value", "nominal", "predictor", "original",
    "y", "numeric", "outcome", "original",
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "key", "original"
  )
  expect_identical(r$var_info, ref_var_info)
})



