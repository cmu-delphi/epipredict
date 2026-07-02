
train_data <- epidatasets::cases_deaths_subset

test_that("autoplot renders a single-target-date forecast (geom_linerange band)", {
  fc <- arx_forecaster(
    train_data,
    "death_rate_7d_av",
    c("death_rate_7d_av", "case_rate_7d_av"),
    args_list = arx_args_list(ahead = 7L, quantile_levels = c(.1, .5, .9))
  )
  p <- autoplot(fc, observed_response = train_data)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("autoplot renders a multi-target-date forecast (geom_ribbon band)", {

  forecast_date <- as.Date("2021-08-01")

  # arx_forecaster only takes a scalar `ahead`, so a real multi-horizon
  # forecast means forecasting each ahead separately and combining them.
  all_canned_results <- lapply(
    seq(0, 28),
    \(days_ahead) {
      arx_forecaster(
        train_data |>
          filter(time_value <= forecast_date),
        outcome = "death_rate_7d_av",
        predictors = c("case_rate_7d_av", "death_rate_7d_av"),
        trainer = quantile_reg(),
        args_list = arx_args_list(
          lags = list(c(0, 1, 2, 3, 7, 14), c(0, 7, 14)),
          ahead = days_ahead
        )
      )
    }
  )
  # pull out the workflow and the predictions to be able to use autoplot
  workflow <- all_canned_results[[1]]$epi_workflow
  results <- all_canned_results |>
    purrr::map(~ `$`(., "predictions")) |>
    purrr::list_rbind()
  p <- autoplot(
    object = workflow,
    predictions = results,
    observed_response = train_data |>
      filter(time_value > "2021-07-01")
  )
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})
