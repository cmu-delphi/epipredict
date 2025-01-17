test_that("roll_modular_multivec works", {
  tib <- tibble(
    col = c(1, 2, 3, 3.5, 4, 1, -2, 4, 1, 0),
    indx = c(1, 1, 1, 2, 2, 3, 3, 3, 1, 1),
    w = rep(1, 10)
  )
  modulus <- 3L
  # unweighted mean
  expected_res <- tib |>
    mutate(index = indx %% modulus, index = index + (index == 0) * modulus ) |>
    summarise(climate_pred = weighted.mean(col, w = w), .by = index)
  expect_equal(
    roll_modular_multivec(tib$col, tib$indx, tib$w, "mean", 0, modulus),
    expected_res
  )
  w_size <- 1L
  expected_res <- tibble(index = as.double(1:3), climate_pred = mean(tib$col))
  expect_equal(
    roll_modular_multivec(tib$col, tib$indx, tib$w, "mean", 1L, modulus),
    expected_res
  )
  # weighted mean
  tib$w <- c(1, 2, 3, 1, 2, 1, 1, 2, 2, 1)
  expected_res <- tib |>
    mutate(index = indx %% modulus, index = index + (index == 0) * modulus ) |>
    summarise(climate_pred = weighted.mean(col, w = w), .by = index)
  expect_equal(
    roll_modular_multivec(tib$col, tib$indx, tib$w, "mean", 0, modulus),
    expected_res
  )
  tib$w <- c(1, 2, 3, 1, 2, 1, 1, 2, 2, 1)
  expected_res <- tibble(
    index = as.double(1:3),
    climate_pred = weighted.mean(tib$col, tib$w)
  )
  expect_equal(
    roll_modular_multivec(tib$col, tib$indx, tib$w, "mean", 1L, modulus),
    expected_res
  )
  # median
  expected_res <- tib |>
    mutate(index = indx %% modulus, index = index + (index == 0) * modulus ) |>
    summarise(climate_pred = median(col), .by = index)
  expect_equal(
    roll_modular_multivec(tib$col, tib$indx, tib$w, "median", 0, modulus),
    expected_res
  )
  expected_res <- tibble(index = as.double(1:3), climate_pred = median(tib$col))
  expect_equal(
    roll_modular_multivec(tib$col, tib$indx, tib$w, "median", 1L, modulus),
    expected_res
  )
})
