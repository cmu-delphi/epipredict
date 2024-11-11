train_data <- cases_deaths_subset
expect_snapshot_tibble <- function(x) {
  expect_snapshot_value(x, style = "deparse", cran = FALSE)
}

test_that("flatline_forecaster snapshots", {
  # Let's make a few forecasts using different settings and snapshot them
  flat1 <- flatline_forecaster(train_data, "death_rate_7d_av")
  expect_snapshot_tibble(flat1$predictions)

  flat2 <- flatline_forecaster(
    train_data, "death_rate_7d_av",
    args_list = flatline_args_list(ahead = 1L)
  )
  expect_snapshot_tibble(flat2$predictions)

  flat3 <- flatline_forecaster(
    train_data, "death_rate_7d_av",
    args_list = flatline_args_list(
      forecast_date = as.Date("2021-12-31")
    )
  )
  expect_snapshot_tibble(flat3$predictions)

  flat4 <- flatline_forecaster(
    train_data, "death_rate_7d_av",
    args_list = flatline_args_list(
      target_date = as.Date("2022-01-01"),
    )
  )
  expect_snapshot_tibble(flat4$predictions)
})

test_that("cdc_baseline_forecaster snapshots", {
  set.seed(1234)
  cdc1 <- cdc_baseline_forecaster(train_data, "death_rate_7d_av")
  expect_snapshot_tibble(cdc1$predictions)

  cdc2 <- cdc_baseline_forecaster(
    train_data, "death_rate_7d_av",
    args_list = cdc_baseline_args_list(aheads = 2:6)
  )
  expect_snapshot_tibble(cdc2$predictions)

  cdc3 <- cdc_baseline_forecaster(
    train_data, "death_rate_7d_av",
    args_list = cdc_baseline_args_list(
      data_frequency = "5 days",
    )
  )
  expect_snapshot_tibble(cdc3$predictions)
})

test_that("arx_forecaster snapshots", {
  arx1 <- arx_forecaster(
    train_data,
    "death_rate_7d_av",
    c("death_rate_7d_av", "case_rate_7d_av")
  )
  expect_snapshot_tibble(arx1$predictions)

  arx2 <- arx_forecaster(
    train_data,
    "death_rate_7d_av",
    c("death_rate_7d_av", "case_rate_7d_av"),
    args_list = arx_args_list(
      ahead = 1L
    )
  )
  expect_snapshot_tibble(arx2$predictions)
})

test_that("arx_classifier snapshots", {
  arc1 <- arx_classifier(
    case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-11-01")),
    "death_rate",
    c("case_rate", "death_rate")
  )
  expect_snapshot_tibble(arc1$predictions)
})
