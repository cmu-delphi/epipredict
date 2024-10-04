time_values <- as.Date("2021-01-01") + +floor(seq(0, 100, by = .5))[1:200]
as_of <- max(time_values) + 5
max_time <- max(time_values)
old_data <- tibble(
  geo_value = rep(c("place1", "place2"), 100),
  time_value = as.Date("2021-01-01") + +floor(seq(0, 100, by = .5))[1:200],
  case_rate = sqrt(1:200) + atan(0.1 * 1:200) + sin(5 * 1:200) + 1,
  tmp_death_rate = atan(0.1 * 1:200) + cos(5 * 1:200) + 1
) %>%
  # place2 is slightly more recent than place1
  mutate(time_value = as.Date(ifelse(geo_value == "place2", time_value + 1, time_value))) %>%
  as_epi_df(as_of = as_of)
old_data
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
    epi_shift_single(old_data, "case_rate", -4, "case_rate_a", keys),
    by = keys
  ) %>%
  dplyr::full_join(
    epi_shift_single(old_data, "case_rate", 3, "case_rate_b", keys),
    by = keys
  ) %>%
  dplyr::full_join(
    epi_shift_single(old_data, "death_rate", 7, "death_rate_a", keys),
    by = keys
  ) %>%
  arrange(time_value)
time_range <- as.Date("2021-01-01") + 0:199
x_adjust_ahead <- tibble(
  geo_value = rep("place", 200),
  time_value = time_range,
  case_rate = sqrt(1:200) + atan(0.1 * 1:200) + sin(5 * 1:200) + 1,
  death_rate = atan(0.1 * 1:200) + cos(5 * 1:200) + 1
) %>%
  as_epi_df(as_of = max(time_range) + 3)

modified_data %>% arrange(geo_value, desc(time_value))
modified_data %>%
  group_by(geo_value) %>%
  filter(!is.na(case_rate)) %>%
  summarise(max(time_value))
as_of

toy_df <- tribble(
  ~geo_value, ~time_value, ~a, ~b,
  "ma", as.Date("2015-01-11"), 20, 6,
  "ma", as.Date("2015-01-12"), 23, NA,
  "ma", as.Date("2015-01-13"), 25, NA,
  "ca", as.Date("2015-01-11"), 100, 5,
  "ca", as.Date("2015-01-12"), 103, 10,
) %>%
  as_epi_df(as_of = as.Date("2015-01-14"))

test_that("get_latency works", {
  expect_equal(get_latency(modified_data, as_of, "case_rate", 1, "geo_value"), 5)
  expect_equal(get_latency(modified_data, as_of, "case_rate", -1, "geo_value"), -5)
  expect_equal(get_latency(modified_data, as_of, "death_rate", 1, "geo_value"), 4)
  expect_equal(get_latency(modified_data, as_of, "case_rate_a", 1, "geo_value"), 5 + 4)
  expect_equal(get_latency(modified_data, as_of, "case_rate_b", 1, "geo_value"), 5 - 3)
  expect_equal(get_latency(modified_data, as_of, "death_rate_a", 1, "geo_value"), 4 - 7)
  expect_equal(get_latency(toy_df, as.Date("2015-01-14"), "a", 1, "geo_value"), 2)
  expect_equal(get_latency(toy_df, as.Date("2015-01-14"), "a", -1, "geo_value"), -2)
  expect_equal(get_latency(toy_df, as.Date("2015-01-14"), "b", 1, "geo_value"), 3)
  expect_equal(get_latency(toy_df, as.Date("2015-01-14"), "b", -1, "geo_value"), -3)
})

test_that("get_latency infers max_time to be the minimum `max time` across grouping the specified keys", {
  # place 2 is already 1 day less latent than place 1, so decreasing it's
  # latency it should have no effect
  place2_delayed_data <- modified_data %>% mutate(time_value = time_value + 3 * (geo_value == "place2"))
  expect_equal(get_latency(place2_delayed_data, as_of, "case_rate", 1, "geo_value"), 5)
  # decreaseing the latency of place1 more than 1 pushes it past place2, so at most changes the latency by 1
  place1_delayed_data <- modified_data %>% mutate(time_value = time_value + 5 * (geo_value == "place1"))
  expect_equal(get_latency(place1_delayed_data, as_of, "case_rate", 1, "geo_value"), 4)
})


test_that("get_forecast_date works", {
  info <- tribble(
    ~variable, ~type, ~role, ~source,
    "time_value", "date", "time_value", "original",
    "geo_value", "nominal", "geo_value", "original",
    "case_rate", "numeric", "raw", "original",
    "death_rate", "numeric", "raw", "original",
    "not_real", "numeric", "predictor", "derived"
  )
  expect_equal(get_forecast_date(modified_data, info, "geo_value", NULL), as_of)
  expect_equal(get_forecast_date(modified_data, info, "", NULL), as_of)
  expect_equal(get_forecast_date(modified_data, info, NULL, NULL), as_of)
})

test_that("pad_to_end works correctly", {
  single_ex <- tribble(
    ~geo_value, ~time_value, ~a, ~b,
    "1", as.Date("1066-10-13"), 2, -.6,
    # internal NA
    "1", as.Date("1066-10-14"), NA, NA,
    "1", as.Date("1066-10-15"), 1, -.5,
    "2", as.Date("1066-10-13"), 3, .9,
    # note these are intentionally out of order
    "3", as.Date("1066-10-14"), 2.5, NA,
    "3", as.Date("1066-10-13"), 2, -.6,
  ) %>%
    as_epi_df(as_of = "1066-10-16")
  expect_equal(
    single_ex %>% pad_to_end("geo_value", as.Date("1066-10-16")),
    rbind(
      single_ex[-5, ],
      tibble(geo_value = "1", time_value = as.Date("1066-10-16"), a = 1, b = -.5),
      tibble(
        geo_value = "2",
        time_value = seq.Date(
          from = as.Date("1066-10-14"),
          to = as.Date("1066-10-16"),
          by = 1
        ),
        a = 3, b = .9
      ),
      tibble(
        geo_value = "3",
        time_value = seq.Date(
          from = as.Date("1066-10-14"),
          to = as.Date("1066-10-16"),
          by = 1
        ),
        a = 2.5, b = -0.6
      )
    ) %>% arrange(geo_value, time_value)
  )
})


test_that("pad_to_end handles weeks", {
  single_ex <- tribble(
    ~geo_value, ~time_value, ~a, ~b,
    "1", as.Date("1066-10-14"), 2, -.6,
    "1", as.Date("1066-10-21"), 1, -.5,
    "2", as.Date("1066-10-14"), 3, .9
  ) %>%
    as_epi_df(as_of = "1066-10-28")
  expect_equal(
    single_ex %>% pad_to_end("geo_value", as.Date("1066-10-28")),
    rbind(
      single_ex,
      tibble(geo_value = "1", time_value = as.Date("1066-10-28"), a = 1, b = -.5),
      tibble(
        geo_value = "2",
        time_value = seq.Date(
          from = as.Date("1066-10-21"),
          to = as.Date("1066-10-28"),
          by = 7
        ),
        a = 3, b = .9
      )
    ) %>% arrange(geo_value, time_value)
  )
})
# todo case where somehow columns of different roles are selected
