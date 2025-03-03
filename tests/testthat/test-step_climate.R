test_that("yday_leap works", {
  # feburary 29th is assigned a negative value
  expect_equal(yday_leap(as.Date("2024-02-29")), 999)
  # before that is normal
  expect_equal(yday_leap(as.Date("2024-02-28")), 31 + 28)

  # after that is decreased by 1 (so matches non leap years)
  expect_equal(
    yday_leap(as.Date("2024-05-28")),
    lubridate::yday(as.Date("2022-05-28"))
  )
  # off leap years have the right value
  expect_equal(yday_leap(as.Date("2023-05-28")), 31 + 28 + 31 + 30 + 28)
})
test_that("roll_modular_multivec works", {
  tib <- tibble(
    col = c(1, 2, 3, 3.5, 4, 1, -2, 4, 1, 0),
    .idx = c(1, 1, 1, 2, 2, 3, 3, 3, 1, 1),
    w = rep(1, 10)
  )
  modulus <- 3L

  Mean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  Median <- function(x, w) median(x, na.rm = TRUE)

  # unweighted mean
  # window of size 0
  expected_res <- tib |>
    mutate(.idx = .idx %% modulus, .idx = .idx + (.idx == 0) * modulus) |>
    summarise(climate_pred = weighted.mean(col, w = w), .by = .idx)
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, Mean, 0, modulus),
    expected_res
  )
  # window of size 1, which includes everything
  expected_res <- tibble(.idx = as.double(1:3), climate_pred = mean(tib$col))
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, Mean, 1L, modulus),
    expected_res
  )

  # weighted mean
  # window of size 0
  tib$w <- c(1, 2, 3, 1, 2, 1, 1, 2, 2, 1)
  expected_res <- tib |>
    mutate(.idx = .idx %% modulus, .idx = .idx + (.idx == 0) * modulus) |>
    summarise(climate_pred = weighted.mean(col, w = w), .by = .idx)
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, Mean, 0, modulus),
    expected_res
  )
  # window of size 1
  expected_res <- tibble(
    .idx = as.double(1:3),
    climate_pred = weighted.mean(tib$col, tib$w)
  )
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, Mean, 1L, modulus),
    expected_res
  )
  # median
  expected_res <- tib |>
    mutate(.idx = .idx %% modulus, .idx = .idx + (.idx == 0) * modulus) |>
    summarise(climate_pred = median(col), .by = .idx)
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, Median, 0, modulus),
    expected_res
  )
  expected_res <- tibble(.idx = as.double(1:3), climate_pred = median(tib$col))
  expect_equal(
    roll_modular_multivec(tib$col, tib$.idx, tib$w, Median, 1L, modulus),
    expected_res
  )
})

test_that("prep/bake steps create the correct training data", {
  single_yr <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
    # shift by 2 days to match the epiweeks of 2021
    y = rep(c(1, 1, rep(c(1:26, 26:2), each = 7), 1, 1, 1, 1, 1, 1), times = 2L)
  ) %>%
    as_epi_df()
  # epiweeks 1, 52, and 53 are all 1, note that there are days in wk 52, 2 in wk 53
  r <- epi_recipe(x) %>% step_climate(y, time_type = "epiweek")
  p <- prep(r, x)

  expected_res <- tibble(.idx = c(1:52, 999), climate_y = c(2, 2:25, 25, 25, 25:2, 2, 2))
  expect_equal(p$steps[[1]]$climate_table, expected_res)

  b <- bake(p, new_data = NULL)
  expected_bake <- x %>%
    mutate(.idx = epiweek_leap(time_value)) %>%
    left_join(expected_res, by = join_by(.idx)) %>%
    select(-.idx)
  expect_equal(b, expected_bake)
})

test_that("prep/bake steps create the correct training data with an incomplete year", {
  single_yr <- seq(as.Date("2021-01-01"), as.Date("2021-10-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
    # shift by 2 days to match the epiweeks of 2021
    y = rep(c(1, 1, rep(c(1:26, 26:2), each = 7), 1, 1, 1, 1, 1, 1)[1:length(single_yr)], times = 2L)
  ) %>%
    as_epi_df()
  # epiweeks 1, 52, and 53 are all 1, note that there are days in wk 52, 2 in wk 53
  r <- epi_recipe(x) %>% step_climate(y, time_type = "epiweek")
  p <- prep(r, x)

  expected_res <- tibble(.idx = c(1:44, 999), climate_y = c(2, 3, 3, 4:25, 25, 25, 25:12, 12, 11, 11, 10))
  expect_equal(p$steps[[1]]$climate_table, expected_res)

  b <- bake(p, new_data = NULL)
  expected_bake <- x %>%
    mutate(.idx = epiweek_leap(time_value)) %>%
    left_join(expected_res, by = join_by(.idx)) %>%
    select(-.idx)
  expect_equal(b, expected_bake)
})

test_that("prep/bake steps create the correct training data for non leapweeks", {
  single_yr <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
    # shift by 2 days to match the epiweeks of 2021
    y = rep(c(1, 1, rep(c(1:26, 26:2), each = 7), 1, 1, 1, 1, 1, 1), times = 2L)
  ) %>%
    as_epi_df()
  # epiweeks 1, 52, and 53 are all 1, note that there are days in wk 52, 2 in wk 53
  r <- epi_recipe(x) %>% step_climate(y, time_type = "epiweek")
  p <- prep(r, x)

  expected_res <- tibble(.idx = 1:52, climate_y = c(2, 2:25, 25, 25, 25:2, 2))
  expect_equal(p$steps[[1]]$climate_table, expected_res)

  b <- bake(p, new_data = NULL)
  expected_bake <- x %>%
    mutate(.idx = lubridate::epiweek(time_value)) %>%
    left_join(expected_res, by = join_by(.idx)) %>%
    select(-.idx)
  expect_equal(b, expected_bake)
})

test_that("prep/bake steps create the correct training data months", {
  single_yr <- seq(as.Date("2021-01-01"), as.Date("2023-12-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
  ) %>%
    # 1 2 3 4 5 6 6 5 4 3 2 1, assigned based on the month
    mutate(y = pmin(13 - month(time_value), month(time_value))) %>%
    as_epi_df()

  # epiweeks 1, 52, and 53 are all 1, note that there are days in wk 52, 2 in wk 53
  r <- epi_recipe(x) %>% step_climate(y, time_type = "month", window_size = 1)
  p <- prep(r, x)

  expected_res <- tibble(.idx = 1:12, climate_y = c(1:6, 6:1))
  expect_equal(p$steps[[1]]$climate_table, expected_res)

  b <- bake(p, new_data = NULL)
  expected_bake <- x %>%
    mutate(.idx = month(time_value)) %>%
    left_join(expected_res, by = join_by(.idx)) %>%
    select(-.idx)
  expect_equal(b, expected_bake)
})


test_that("prep/bake steps create the correct training data for daily data", {
  single_yr <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
    y = rep(c(1:183, 184:2), times = 2L)
  ) %>%
    as_epi_df()
  # epiweeks 1, 52, and 53 are all 1, note that there are days in wk 52, 2 in wk 53
  r <- epi_recipe(x) %>% step_climate(y, time_type = "day")
  p <- prep(r, x)

  expected_res <- tibble(
    .idx = c(1:365, 999),
    climate_y = c(3, 3, 3:(59 - 4), 56.5:63.5, 65:181, rep(182, 5), 181:3, 3, 59)
  )
  expect_equal(p$steps[[1]]$climate_table, expected_res)

  b <- bake(p, new_data = NULL)
  expected_bake <- x %>%
    mutate(.idx = yday_leap(time_value)) %>%
    left_join(expected_res, by = join_by(.idx)) %>%
    select(-.idx)
  expect_equal(b, expected_bake)
})


test_that("leading the climate predictor works as expected", {
  single_yr <- seq(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "1 day")
  x <- tibble(
    time_value = rep(single_yr, times = 2L),
    geo_value = rep(c("reg1", "reg2"), each = length(single_yr)),
    # shift by 2 days to match the epiweeks of 2021
    y = rep(c(1, 1, rep(c(1:26, 26:2), each = 7), 1, 1, 1, 1, 1, 1), times = 2L)
  ) %>%
    as_epi_df()
  # epiweeks 1, 52, and 53 are all 1, note that there are days in wk 52, 2 in wk 53
  r <- epi_recipe(x) %>%
    step_epi_ahead(y, ahead = 14L) %>%
    step_epi_lag(y, lag = c(0, 7L, 14L)) %>%
    step_climate(y, forecast_ahead = 2L, time_type = "epiweek") %>%
    # matches the response
    step_epi_naomit()
  p <- prep(r, x)

  expected_res <- tibble(.idx = c(1:52, 999), climate_y = c(2, 2, 3, 4, 4.5, 5.5, 7:25, 25, 25, 25:2, 2, 2)) %>%
    mutate(
      climate_y = climate_y[c(3:53, 1:2)]
    ) %>%
    arrange(.idx)
  expect_equal(p$steps[[3]]$climate_table, expected_res)

  b <- bake(p, new_data = NULL)
  expect_identical(max(b$time_value), as.Date("2021-12-17")) # last date with no NAs
  # expected climate predictor should be shifted forward by 2 weeks
  expected_climate_pred <- x %>%
    mutate(
      .idx = lubridate::epiweek(time_value) %% 53,
      .idx = dplyr::case_when(.idx == 0 ~ 53, TRUE ~ .idx)
    ) %>%
    left_join(expected_res, by = join_by(.idx)) %>%
    arrange(time_value, geo_value) %>%
    filter(time_value %in% unique(b$time_value)) %>%
    pull(climate_y)
  expect_identical(b$climate_y, expected_climate_pred)

  # Check if our test data has the right values
  td <- get_test_data(r, x)
  expected_test_x <- td %>%
    filter(time_value == "2021-12-31") %>%
    mutate(
      ahead_14_y = NA_real_, lag_0_y = 1, lag_7_y = 2, lag_14_y = 3,
      climate_y = 2
    )
  expect_equal(bake(p, td), expected_test_x)
})
