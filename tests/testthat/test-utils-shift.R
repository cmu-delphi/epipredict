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
  expect_snapshot(adjust_latency(object, x_adjust_ahead))
})
