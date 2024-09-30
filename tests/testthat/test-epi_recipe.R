test_that("epi_recipe produces default recipe", {
  # these all call recipes::recipe(), but the template will always have 1 row
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5)
  )
  expected_rec <- recipes::recipe(tib)
  expected_rec$template <- expected_rec$template[1, ]
  expect_warning(rec <- epi_recipe(tib), regexp = "epi_recipe has been called with a non-epi_df object")
  expect_identical(expected_rec, rec)
  expect_equal(nrow(rec$template), 1L)

  expected_rec <- recipes::recipe(y ~ x, tib)
  expected_rec$template <- expected_rec$template[1, ]
  expect_warning(rec <- epi_recipe(y ~ x, tib), regexp = "epi_recipe has been called with a non-epi_df object")
  expect_identical(expected_rec, rec)
  expect_equal(nrow(rec$template), 1L)

  m <- as.matrix(tib)
  expected_rec <- recipes::recipe(m)
  expected_rec$template <- expected_rec$template[1, ]
  expect_warning(rec <- epi_recipe(m), regexp = "epi_recipe has been called with a non-epi_df object")
  expect_identical(expected_rec, rec)
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
    ~variable, ~type, ~role, ~source,
    "x", c("integer", "numeric"), "predictor", "original",
    "y", c("integer", "numeric"), "outcome", "original",
    "time_value", "date", "time_value", "original",
    "geo_value", c("string", "unordered", "nominal"), "geo_value", "original"
  )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)

  # with an epi_key as a predictor
  r <- epi_recipe(y ~ x + geo_value, tib)
  ref_var_info <- ref_var_info %>%
    tibble::add_row(
      variable = "geo_value", type = list(c("string", "unordered", "nominal")),
      role = "predictor",
      source = "original", .after = 1
    )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)

  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
    geo_value = "ca",
    z = "dummy_key"
  ) %>% epiprocess::as_epi_df(other_keys = "z")

  # with an additional key
  r <- epi_recipe(y ~ x + geo_value, tib)
  ref_var_info <- ref_var_info %>%
    tibble::add_row(
      variable = "z", type = list(c("string", "unordered", "nominal")),
      role = "key",
      source = "original"
    )

  expect_identical(r$var_info, ref_var_info)
})

test_that("epi_recipe epi_df works", {
  tib <- tibble(
    x = 1:5, y = 1:5,
    time_value = seq(as.Date("2020-01-01"), by = 1, length.out = 5),
    geo_value = "ca"
  ) %>% epiprocess::as_epi_df()

  r <- epi_recipe(tib)
  ref_var_info <- tibble::tribble(
    ~variable, ~type, ~role, ~source,
    "time_value", "date", "time_value", "original",
    "geo_value", c("string", "unordered", "nominal"), "geo_value", "original",
    "x", c("integer", "numeric"), "raw", "original",
    "y", c("integer", "numeric"), "raw", "original"
  )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)

  r <- epi_recipe(tib, formula = y ~ x)
  ref_var_info <- tibble::tribble(
    ~variable, ~type, ~role, ~source,
    "x", c("integer", "numeric"), "predictor", "original",
    "y", c("integer", "numeric"), "outcome", "original",
    "time_value", "date", "time_value", "original",
    "geo_value", c("string", "unordered", "nominal"), "geo_value", "original"
  )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)


  r <- epi_recipe(
    tib,
    roles = c("geo_value", "funny_business", "predictor", "outcome")
  )
  ref_var_info <- ref_var_info %>%
    tibble::add_row(
      variable = "time_value", type = list("date"), role = "funny_business",
      source = "original"
    )
  expect_identical(r$var_info, ref_var_info)
  expect_equal(nrow(r$template), 1L)
})


test_that("add/update/adjust/remove epi_recipe works as intended", {
  jhu <- covid_case_death_rates

  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14))

  wf <- epi_workflow() %>%
    add_epi_recipe(r)

  steps <- workflows::extract_preprocessor(wf)$steps
  expect_equal(length(steps), 3)
  expect_equal(class(steps[[1]]), c("step_epi_lag", "step"))
  expect_equal(steps[[1]]$lag, c(0, 7, 14))
  expect_equal(class(steps[[2]]), c("step_epi_ahead", "step"))
  expect_equal(steps[[2]]$ahead, c(7))
  expect_equal(class(steps[[3]]), c("step_epi_lag", "step"))
  expect_equal(steps[[3]]$lag, c(0, 7, 14))

  r2 <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 1)) %>%
    step_epi_ahead(death_rate, ahead = 1)

  wf <- update_epi_recipe(wf, r2)

  steps <- workflows::extract_preprocessor(wf)$steps
  expect_equal(length(steps), 2)
  expect_equal(class(steps[[1]]), c("step_epi_lag", "step"))
  expect_equal(steps[[1]]$lag, c(0, 1))
  expect_equal(class(steps[[2]]), c("step_epi_ahead", "step"))
  expect_equal(steps[[2]]$ahead, c(1))

  # adjust_epi_recipe using step number
  wf <- adjust_epi_recipe(wf, which_step = 2, ahead = 7)
  steps <- workflows::extract_preprocessor(wf)$steps
  expect_equal(length(steps), 2)
  expect_equal(class(steps[[1]]), c("step_epi_lag", "step"))
  expect_equal(steps[[1]]$lag, c(0, 1))
  expect_equal(class(steps[[2]]), c("step_epi_ahead", "step"))
  expect_equal(steps[[2]]$ahead, c(7))

  # adjust_epi_recipe using step name
  wf <- adjust_epi_recipe(wf, which_step = "step_epi_ahead", ahead = 8)
  steps <- workflows::extract_preprocessor(wf)$steps
  expect_equal(length(steps), 2)
  expect_equal(class(steps[[1]]), c("step_epi_lag", "step"))
  expect_equal(steps[[1]]$lag, c(0, 1))
  expect_equal(class(steps[[2]]), c("step_epi_ahead", "step"))
  expect_equal(steps[[2]]$ahead, c(8))


  wf <- remove_epi_recipe(wf)
  expect_error(workflows::extract_preprocessor(wf)$steps)
  expect_equal(wf$pre$actions$recipe$recipe, NULL)
})
