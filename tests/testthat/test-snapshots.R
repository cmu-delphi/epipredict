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
  attributes(train_data)$metadata$as_of <- max(train_data$time_value) + 5
  arx3 <- arx_forecaster(
    train_data,
    "death_rate_7d_av",
    c("death_rate_7d_av", "case_rate_7d_av"),
    args_list = arx_args_list(
      ahead = 1L,
      adjust_latency = "extend_ahead"
    )
  )
  # consistency check
  expect_snapshot_tibble(arx3$predictions)
  expect_equal(
    arx3$predictions$target_date,
    rep(attributes(train_data)$metadata$as_of + 1, times = 6)
  )
  expect_equal(
    arx3$predictions$target_date,
    arx2$predictions$target_date + 5
  )
  expect_equal(
    arx3$predictions$forecast_date,
    arx2$predictions$forecast_date + 5
  )
  # not the same predictions
  expect_false(all(arx2$predictions == arx3$predictions))


  arx4 <- arx_forecaster(
    train_data,
    "death_rate_7d_av",
    c("death_rate_7d_av", "case_rate_7d_av"),
    args_list = arx_args_list(
      ahead = 1L,
      adjust_latency = "locf"
    )
  )
  # consistency check
  expect_snapshot_tibble(arx3$predictions)
})

test_that("arx_forecaster output format snapshots", {
  jhu <- epidatasets::covid_case_death_rates %>%
    dplyr::filter(time_value >= as.Date("2021-12-01"))
  attributes(jhu)$metadata$as_of <- as.Date(attributes(jhu)$metadata$as_of)
  out1 <- arx_forecaster(
    jhu, "death_rate",
    c("case_rate", "death_rate")
  )
  expect_equal(as.Date(format(out1$metadata$forecast_created, "%Y-%m-%d")), Sys.Date())
  out1$metadata$forecast_created <- as.Date("1999-01-01")
  expect_snapshot(out1)
  out2 <- arx_forecaster(jhu, "case_rate",
    c("case_rate", "death_rate"),
    trainer = quantile_reg(),
    args_list = arx_args_list(
      quantile_levels = 1:9 / 10,
      adjust_latency = "extend_lags",
      forecast_date = as.Date("2022-01-03")
    )
  )
  expect_equal(as.Date(format(out2$metadata$forecast_created, "%Y-%m-%d")), Sys.Date())
  out2$metadata$forecast_created <- as.Date("1999-01-01")
  expect_snapshot(out2)
  out3 <- arx_forecaster(jhu, "death_rate",
    c("case_rate", "death_rate"),
    trainer = quantile_reg(),
    args_list = arx_args_list(
      adjust_latency = "extend_ahead",
      forecast_date = as.Date("2022-01-03")
    )
  )
  expect_equal(as.Date(format(out3$metadata$forecast_created, "%Y-%m-%d")), Sys.Date())
  out3$metadata$forecast_created <- as.Date("1999-01-01")
  expect_snapshot(out3)
})

test_that("arx_classifier snapshots", {
  arc1 <- arx_classifier(
    case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-11-01")),
    "death_rate",
    c("case_rate", "death_rate")
  )
  expect_snapshot_tibble(arc1$predictions)
  max_date <- case_death_rate_subset$time_value %>% max()
  arc2 <- arx_classifier(
    case_death_rate_subset %>%
      dplyr::filter(time_value >= as.Date("2021-11-01")),
    "death_rate",
    c("case_rate", "death_rate"),
    args_list = arx_class_args_list(adjust_latency = "extend_ahead", forecast_date = max_date + 2)
  )
  expect_snapshot_tibble(arc2$predictions)
  expect_error(
    arc3 <- arx_classifier(
      case_death_rate_subset %>%
        dplyr::filter(time_value >= as.Date("2021-11-01")),
      "death_rate",
      c("case_rate", "death_rate"),
      args_list = arx_class_args_list(adjust_latency = "extend_lags", forecast_date = max_date + 2)
    ),
    class = "epipredict__arx_classifier__adjust_latency_unsupported_method"
  )
  expect_error(
    arc4 <- arx_classifier(
      case_death_rate_subset %>%
        dplyr::filter(time_value >= as.Date("2021-11-01")),
      "death_rate",
      c("case_rate", "death_rate"),
      args_list = arx_class_args_list(adjust_latency = "locf", forecast_date = max_date + 2)
    ),
    class = "epipredict__arx_classifier__adjust_latency_unsupported_method"
  )
})
