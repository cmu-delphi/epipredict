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
  ~terms, ~shift, ~prefix, ~original_name, ~latency, ~effective_shift, ~new_name, ~type, ~role,
  "case_rate", 3, "lag_", "lag_3_case_rate", 5, 8, "lag_8_case_rate", "numeric", "predictor",
  "death_rate", 7, "lag_", "lag_7_death_rate", 4, 11, "lag_11_death_rate", "numeric", "predictor",
  "case_rate", 4, "ahead_", "ahead_4_case_rate", -5, 9, "ahead_9_case_rate", "numeric", "outcome"
)
test_recipe <- epi_recipe(modified_data) %>%
  step_epi_lag(case_rate, lag = c(3)) %>%
  step_epi_lag(death_rate, lag = 7) %>%
  step_epi_ahead(case_rate, ahead = 4)
shift_cols <- construct_shift_tibble(c("case_rate", "death_rate"), test_recipe, "step_epi_lag", "lag")
test_that("construct_shift_tibble constructs the right tibble", {
  expected_shift_cols <- tibble::tribble(
    ~terms, ~shift, ~prefix,
    "case_rate", 3, "lag_",
    "death_rate", 7, "lag_"
  )
  expect_equal(shift_cols, expected_shift_cols)
})

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

test_that("get_asof works", {
  info <- tribble(
    ~variable, ~type, ~role, ~source,
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "geo_value", "original",
    "case_rate", "numeric", "raw", "original",
    "death_rate", "numeric", "raw", "original",
    "not_real", "numeric", "predictor", "derived"
  )
  expect_equal(set_forecast_date(modified_data, info), as_of)
})

test_that("get_latent_column_tibble infers latency and works correctly", {
  info <- tibble(variable = c("lag_3_case_rate", "lag_7_death_rate", "ahead_4_case_rate"), type = "numeric", role = c(rep("predictor", 2), "outcome"), source = "derived")

  case_lag <- get_latent_column_tibble(
    shift_cols[1, ], modified_data, as_of, NULL, 1, info
  )
  expect_equal(case_lag, all_shift_cols[1, ])

  death_lag <- get_latent_column_tibble(
    shift_cols[2, ], modified_data, as_of, NULL, 1, info
  )
  expect_equal(death_lag, all_shift_cols[2, ])

  both_lag <- get_latent_column_tibble(
    shift_cols, modified_data, as_of, NULL, 1, info
  )
  expect_equal(both_lag, all_shift_cols[1:2, ])
})

test_that("get_latent_column_tibble assigns given latencies", {
  # non-null latency
  info <- tibble(variable = c("lag_3_case_rate", "lag_7_death_rate", "ahead_4_case_rate"), type = "numeric", role = c(rep("predictor", 2), "outcome"), source = "derived")
  both_lag <- get_latent_column_tibble(
    shift_cols, modified_data, as_of, 50, 1, info
  )
  weird_latencies <- tibble::tribble(
    ~terms, ~shift, ~prefix, ~original_name, ~latency, ~effective_shift, ~new_name, ~type, ~role,
    "case_rate", 3, "lag_", "lag_3_case_rate", 50, 53, "lag_53_case_rate", "numeric", "predictor",
    "death_rate", 7, "lag_", "lag_7_death_rate", 50, 57, "lag_57_death_rate", "numeric", "predictor",
  )
  expect_equal(both_lag, weird_latencies)

  # supposing we add the latencies by hand, and they're different, and in a different order
  weird_latencies <- tibble::tribble(
    ~terms, ~shift, ~prefix, ~original_name, ~latency, ~effective_shift, ~new_name, ~type, ~role,
    "case_rate", 3, "lag_", "lag_3_case_rate", 70, 73, "lag_73_case_rate", "numeric", "predictor",
    "death_rate", 7, "lag_", "lag_7_death_rate", 30, 37, "lag_37_death_rate", "numeric", "predictor",
  )
  both_lag <- get_latent_column_tibble(
    shift_cols, modified_data, as_of, c(death_rate = 30, case_rate = 70), 1, info
  )
  expect_equal(both_lag, weird_latencies[1:2, ])

  ahead_shift_cols <- construct_shift_tibble(c("case_rate"), test_recipe, "step_epi_ahead", "ahead")
  case_ahead <- get_latent_column_tibble(
    ahead_shift_cols, modified_data, as_of, NULL, -1, info
  )
  expect_equal(case_ahead, all_shift_cols[3, ])
})

test_that("get_shifted_column_tibble objects to non-columns", {
  non_shift_cols <- tibble(terms = "not_present", shift = 99, prefix = "lag_")
  expect_error(
    get_latent_column_tibble(
      non_shift_cols, modified_data, as_of, NULL, 1, info
    ),
    regexp = "Can't subset elements that don't exist"
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
    dplyr::add_row(tibble(
      geo_value = "place",
      time_value = as.Date("2021-08-01"), case_rate = NA, death_rate = NA,
      lag_8_case_rate = NA, lag_11_death_rate = NA, ahead_9_case_rate = NA
    )) %>%
    arrange(time_value)
  expect_equal(
    extend_either(modified_data, all_shift_cols, keys) %>% arrange(time_value),
    expected_post_shift
  )
})





time_range <- as.Date("2021-01-01") + 0:199
x_adjust_ahead <- tibble(
  geo_value = rep("place", 200),
  time_value = time_range,
  case_rate = sqrt(1:200) + atan(0.1 * 1:200) + sin(5 * 1:200) + 1,
  death_rate = atan(0.1 * 1:200) + cos(5 * 1:200) + 1
) %>%
  as_epi_df(as_of = max(time_range) + 3)
# confirm the delay is right

test_that("adjust_latency extend_ahead works", {
  # testing that POSIXct converts correctly (as well as basic types)
  expect_equal(
    attributes(x_adjust_ahead)$metadata$as_of - max(x_adjust_ahead$time_value),
    as.difftime(3, units = "days")
  )
  object <- list(latency_adjustment = "extend_ahead", ahead = 7)
  expect_no_error(adjusted_ahead <- adjust_latency(object, x_adjust_ahead))
  expect_type(adjusted_ahead, "integer")
  expect_equal(adjusted_ahead, 3 + 7)
})

test_that("extend_ahead warns in case of extreme adjustment", {
  # warns if the ahead is relatively small
  attributes(x_adjust_ahead)$metadata$as_of <-
    max(x_adjust_ahead$time_value) + 100
  object <- list(latency_adjustment = "extend_ahead", ahead = 7)
  attributes(x_adjust_ahead)$metadata$time_type
  testthat::expect_warning(adjust_latency(object, x_adjust_ahead), regexp = "The ahead has been adjusted by 100")
})

# todo case where somehow columns of different roles are selected
