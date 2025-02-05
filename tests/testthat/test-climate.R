test_that("roll_modular_multivec works", {
  tib <- tibble(
    col = c(1, 2, 3, 3.5, 4, 1, -2, 4, 1, 0),
    .idx = c(1, 1, 1, 2, 2, 3, 3, 3, 1, 1),
    w = rep(1, 10)
  )
  modulus <- 3L
  # unweighted mean
  expected_res <- tib |>
    mutate(.idx = .idx %% modulus, .idx = .idx + (.idx == 0) * modulus ) |>
    summarise(climate_pred = weighted.mean(col, w = w), .by = .idx)
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, "mean", 0, modulus),
    expected_res
  )
  w_size <- 1L
  expected_res <- tibble(.idx = as.double(1:3), climate_pred = mean(tib$col))
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, "mean", 1L, modulus),
    expected_res
  )
  # weighted mean
  tib$w <- c(1, 2, 3, 1, 2, 1, 1, 2, 2, 1)
  expected_res <- tib |>
    mutate(.idx = .idx %% modulus, .idx = .idx + (.idx == 0) * modulus ) |>
    summarise(climate_pred = weighted.mean(col, w = w), .by = .idx)
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, "mean", 0, modulus),
    expected_res
  )
  tib$w <- c(1, 2, 3, 1, 2, 1, 1, 2, 2, 1)
  expected_res <- tibble(
    .idx = as.double(1:3),
    climate_pred = weighted.mean(tib$col, tib$w)
  )
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, "mean", 1L, modulus),
    expected_res
  )
  # median
  expected_res <- tib |>
    mutate(.idx = .idx %% modulus, .idx = .idx + (.idx == 0) * modulus ) |>
    summarise(climate_pred = median(col), .by = .idx)
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, "median", 0, modulus),
    expected_res
  )
  expected_res <- tibble(.idx = as.double(1:3), climate_pred = median(tib$col))
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, "median", 1L, modulus),
    expected_res
  )
})

test_that("bake step creates the correct training data", {
  single_yr <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
    y = rep(dnorm(seq(-3, 3, length = length(single_yr))) * 50 + 5, times = 2L)
  ) %>%
    as_epi_df()
  r <- epi_recipe(x) %>% step_climate(y)
})
