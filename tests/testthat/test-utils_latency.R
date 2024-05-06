time_values <- as.Date("2021-01-01") + 0:199
as_of <- max(time_values) + 5
max_time <- max(time_values)
old_data <- tibble(
  geo_value = rep("place", 200),
  time_value = as.Date("2021-01-01") + 0:199,
  case_rate = sqrt(1:200) + atan(0.1 * 1:200) + sin(5 * 1:200) + 1,
  tmp_death_rate = atan(0.1 * 1:200) + cos(5 * 1:200) + 1
) %>%
  as_epi_df(as_of = as_of)
keys <- c("time_value", "geo_value")
old_data <- old_data %>%
  full_join(epi_shift_single(
    old_data, "tmp_death_rate", 1, "death_rate", keys
  ), by = keys) %>%
  select(-tmp_death_rate)
# old data is created so that death rate has a latency of 4, while case_rate has
# a latency of 5
modified_data <-
  old_data %>%
  dplyr::full_join(
    epi_shift_single(old_data, "case_rate", -4, "ahead_4_case_rate", keys),
    by = keys
  ) %>%
  dplyr::full_join(
    epi_shift_single(old_data, "case_rate", 3, "lag_3_case_rate", keys),
    by = keys
  ) %>%
  dplyr::full_join(
    epi_shift_single(old_data, "death_rate", 7, "lag_7_death_rate", keys),
    by = keys
  ) %>%
  arrange(time_value)
modified_data %>% tail()
as_of - (modified_data %>% filter(!is.na(ahead_4_case_rate)) %>% pull(time_value) %>% max())
all_shift_cols <- tibble::tribble(
  ~original_name, ~shifts, ~latency, ~effective_shift, ~new_name, ~type, ~role,
  "lag_3_case_rate", 3, 5, 8, "lag_8_case_rate", "numeric", "predictor",
  "lag_7_death_rate", 7, 4, 11, "lag_11_death_rate", "numeric", "predictor",
  "ahead_4_case_rate", 4, -5, 9, "ahead_9_case_rate", "numeric", "outcome"
)


test_that("get_latency works", {
  expect_equal(get_latency(modified_data, as_of, "lag_7_death_rate", 7, 1), 4)
  expect_equal(get_latency(modified_data, as_of, "lag_3_case_rate", 3, 1), 5)
  # get_latency does't check the shift_amount
  expect_equal(get_latency(modified_data, as_of, "lag_3_case_rate", 4, 1), 6)
  # ahead works correctly
  expect_equal(get_latency(modified_data, as_of, "ahead_4_case_rate", 4, -1), -5)
  # setting the wrong sign doubles the shift and gets the sign wrong
  expect_equal(get_latency(modified_data, as_of, "ahead_4_case_rate", 4, 1), 5 + 4 * 2)
})

test_that("get_latency infers max_time to be the minimum `max time` across the columns", {})

test_that("adjust_name works", {
  expect_equal(
    adjust_name("lag_", "lag_5_case_rate_13", 10),
    "lag_10_case_rate_13"
  )
  # it won't change a column with the wrong prefix
  expect_equal(
    adjust_name("lag_", "ahead_5_case_rate", 10),
    "ahead_5_case_rate"
  )
  # it works on vectors of names
  expect_equal(
    adjust_name("lag_", c("lag_5_floop_35", "lag_2342352_case"), c(10, 7)),
    c("lag_10_floop_35", "lag_7_case")
  )
})

test_that("get_asof works", {
  info <- tribble(
    ~variable, ~type, ~role, ~source,
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "geo_value", "original",
    "case_rate", "numeric", "raw", "original",
    "death_rate", "numeric", "raw", "original",
    "not_real", "numeric", "predictor", "derived"
  )
  expect_equal(set_asof(modified_data, info), as_of)
})

test_that("get_shifted_column_tibble infers latency and works correctly", {
  info <- tibble(variable = c("lag_3_case_rate", "lag_7_death_rate", "ahead_4_case_rate"), type = "numeric", role = c(rep("predictor", 2), "outcome"), source = "derived")
  case_lag <- get_shifted_column_tibble(
    "lag_", modified_data, "case_rate",
    as_of, NULL, 1, info
  )
  expect_equal(case_lag, all_shift_cols[1, ])

  death_lag <- get_shifted_column_tibble(
    "lag_", modified_data,
    "death_rate", as_of, NULL, 1, info
  )
  expect_equal(death_lag, all_shift_cols[2, ])

  both_lag <- get_shifted_column_tibble(
    "lag_", modified_data, c("case_rate", "death_rate"),
    as_of, NULL, 1, info
  )
  expect_equal(both_lag, all_shift_cols[1:2, ])
})

test_that("get_shifted_column_tibble assigns given latencies", {
  # non-null latency
  info <- tibble(variable = c("lag_3_case_rate", "lag_7_death_rate", "ahead_4_case_rate"), type = "numeric", role = c(rep("predictor", 2), "outcome"), source = "derived")
  both_lag <- get_shifted_column_tibble(
    "lag_", modified_data,
    c("case_rate", "death_rate"), as_of, 50, 1, info
  )
  weird_latencies <- tibble::tribble(
    ~original_name, ~shifts, ~latency, ~effective_shift, ~new_name, ~type, ~role,
    "lag_3_case_rate", 3, 50, 53, "lag_53_case_rate", "numeric", "predictor",
    "lag_7_death_rate", 7, 50, 57, "lag_57_death_rate", "numeric", "predictor",
  )
  expect_equal(both_lag, weird_latencies)

  # supposing we add the latencies by hand, and they're different, and in a different order
  weird_latencies <- tibble::tribble(
    ~original_name, ~shifts, ~latency, ~effective_shift, ~new_name, ~type, ~role,
    "lag_3_case_rate", 3, 70, 73, "lag_73_case_rate", "numeric", "predictor",
    "lag_7_death_rate", 7, 30, 37, "lag_37_death_rate", "numeric", "predictor",
  )
  both_lag <- get_shifted_column_tibble(
    "lag_", modified_data,
    c("case_rate", "death_rate"), as_of, c(death_rate = 30, case_rate = 70), 1, info
  )
  expect_equal(both_lag, weird_latencies[1:2, ])

  case_ahead <- get_shifted_column_tibble(
    "ahead_", modified_data,
    "case_rate", as_of, NULL, -1, info
  )
  expect_equal(case_ahead, all_shift_cols[3, ])
})

test_that("get_shifted_column_tibble objects to non-columns", {
  expect_error(
    get_shifted_column_tibble(
      "lag_", modified_data, "not_present", as_of, NULL, 1, info
    ),
    class = "epipredict_adjust_latency_nonexistent_column_used"
  )
})

test_that("extend_either works", {
  keys <- c("geo_value", "time_value")
  # extend_either doesn't differentiate between the directions, it just moves
  # things
  expected_post_shift <-
    old_data %>%
    dplyr::full_join(
      epi_shift_single(old_data, "case_rate", 8, "lag_8_case_rate", keys),
      by = keys
    ) %>%
    dplyr::full_join(
      epi_shift_single(old_data, "death_rate", 11, "lag_11_death_rate", keys),
      by = keys
    ) %>%
    dplyr::full_join(
      epi_shift_single(old_data, "case_rate", -9, "ahead_9_case_rate", keys),
      by = keys
    ) %>%
    arrange(time_value)
  expect_equal(
    extend_either(modified_data, all_shift_cols, keys) %>% arrange(time_value),
    expected_post_shift
  )
})

# todo case where somehow columns of different roles are selected
