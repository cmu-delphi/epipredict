

test_that("epi_recipe produces default recipe", {
  # these all call recipes::recipe(), but the template will always have 1 row
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5)
  )
  rec <- recipes::recipe(tib)
  rec$template <- rec$template[1,]
  expect_identical(rec, epi_recipe(tib))
  expect_equal(nrow(rec$template), 1L)

  rec <- recipes::recipe(y~x, tib)
  rec$template <- rec$template[1,]
  expect_identical(rec, epi_recipe(y ~ x, tib))
  expect_equal(nrow(rec$template), 1L)

  m <- as.matrix(tib)
  rec <- recipes::recipe(m)
  rec$template <- rec$template[1,]
  expect_identical(rec, epi_recipe(m))
  expect_equal(nrow(rec$template), 1L)

})

test_that("epi_recipe formula works", {

  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
    geo_value = "ca"
  ) %>% epiprocess::as_epi_df()

  # simple case
  r <- epi_recipe(y ~ x, tib)
  ref_var_info <- tibble::tribble(
    ~ variable, ~ type, ~ role, ~ source,
    "x", "numeric", "predictor", "original",
    "y", "numeric", "outcome", "original",
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "geo_value", "original"
  )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)

  # with an epi_key as a predictor
  r <- epi_recipe(y ~ x + geo_value, tib)
  ref_var_info <- ref_var_info %>% tibble::add_row(
    variable = "geo_value", type = "nominal", role = "predictor",
    source = "original", .after = 1)
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)

  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
    geo_value = "ca",
    z = "dummy_key"
  ) %>% epiprocess::as_epi_df(additional_metadata = list(other_keys = "z"))

  # with an additional key
  r <- epi_recipe(y ~ x + geo_value, tib)
  ref_var_info <- ref_var_info %>% tibble::add_row(
    variable = "z", type = "nominal", role = "key",
    source = "original")

  #expect_identical(r$var_info, ref_var_info)

})

test_that("epi_recipe epi_df works", {

  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
    geo_value = "ca"
  ) %>% epiprocess::as_epi_df()

  r <- epi_recipe(tib)
  ref_var_info <- tibble::tribble(
    ~ variable, ~ type, ~ role, ~ source,
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "geo_value", "original",
    "x", "numeric", "raw", "original",
    "y", "numeric", "raw", "original"
  )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)

  r <- epi_recipe(tib, formula = y ~ x)
  ref_var_info <- tibble::tribble(
    ~ variable, ~ type, ~ role, ~ source,
    "x", "numeric", "predictor", "original",
    "y", "numeric", "outcome", "original",
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "geo_value", "original"
  )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)


  r <- epi_recipe(
    tib,
    roles = c("geo_value", "funny_business", "predictor", "outcome")
  )
  ref_var_info <- ref_var_info %>%
    tibble::add_row(
      variable = "time_value", type = "date", role = "funny_business",
      source = "original"
    )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)
})


