library(dplyr)

x <- tibble(
  geo_value = rep("place", 200),
  time_value = as.Date("2021-01-01") + 0:199,
  case_rate = sqrt(1:200) + atan(0.1 * 1:200) + sin(5 * 1:200) + 1,
  death_rate = atan(0.1 * 1:200) + cos(5 * 1:200) + 1
) %>%
  as_epi_df()
max_time <- max(x$time_value)
class(attributes(x)$metadata$as_of)
as_of <- attributes(x)$metadata$as_of
ahead <- 7
latency <- 5

testing_as_of <- max_time + latency
# create x with a plausible as_of date
real_x <- x
attributes(real_x)$metadata$as_of <- testing_as_of

slm_fit <- function(recipe, data = x) {
  epi_workflow() %>%
    add_epi_recipe(recipe) %>%
    add_model(linear_reg()) %>%
    fit(data = data)
}

test_that("epi_adjust_latency correctly extends the lags", {
  r5 <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead) %>%
    step_adjust_latency(method = "extend_lags")
  # the as_of on x is today's date, which is >970 days in the future
  # also, there's no data >970 days in the past, so it gets an error trying to
  # fit on no data
  expect_error(expect_warning(fit5 <- slm_fit(r5), regexp = "The shift has been adjusted by 1022"), class = "simpleError")

  # now trying with the as_of a reasonable distance in the future
  fit5 <- slm_fit(r5, data = real_x)

  expect_equal(
    names(fit5$pre$mold$predictors),
    c(
      "lag_5_death_rate", "lag_11_death_rate", "lag_16_death_rate",
      "lag_6_case_rate", "lag_10_case_rate"
    )
  )
  latest <- get_test_data(r5, x)
  pred <- predict(fit5, latest)
  point_pred <- pred %>% filter(!is.na(.pred))
  expect_equal(nrow(point_pred), 1)
  expect_equal(point_pred$time_value, as.Date(testing_as_of))

  expect_equal(
    names(fit5$pre$mold$outcomes),
    glue::glue("ahead_{ahead}_death_rate")
  )
  latest <- get_test_data(r5, x)
  pred <- predict(fit5, latest)
  actual_solutions <- pred %>% filter(!is.na(.pred))
  expect_equal(actual_solutions$time_value, testing_as_of)

  # should have four predictors, including the intercept
  expect_equal(length(fit5$fit$fit$fit$coefficients), 6)

  # result should be equivalent to just immediately doing the adjusted lags by
  # hand
  hand_adjusted <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(5, 11, 16)) %>%
    step_epi_lag(case_rate, lag = c(6, 10)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  fit_hand_adj <- slm_fit(hand_adjusted, data = real_x)
  expect_equal(
    fit5$fit$fit$fit$coefficients,
    fit_hand_adj$fit$fit$fit$coefficients
  )
})

test_that("epi_adjust_latency correctly extends the ahead", {
  r5 <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead) %>%
    step_adjust_latency(method = "extend_ahead")
  # the as_of on x is today's date, which is >970 days in the future
  # also, there's no data >970 days in the past, so it gets an error trying to
  # fit on no data
  expect_error(expect_warning(fit5 <- slm_fit(r5)))
  # real date example
  fit5 <- slm_fit(r5, data = real_x)
  expect_equal(
    names(fit5$pre$mold$predictors),
    c(
      "lag_0_death_rate", "lag_6_death_rate", "lag_11_death_rate",
      "lag_1_case_rate", "lag_5_case_rate"
    )
  )
  latest <- get_test_data(r5, x)
  pred <- predict(fit5, latest)
  point_pred <- pred %>% filter(!is.na(.pred))
  # max time is still the forecast date
  expect_equal(point_pred$time_value, as.Date(max_time))
  # target column renamed
  expect_equal(
    names(fit5$pre$mold$outcomes),
    glue::glue("ahead_{ahead + latency}_death_rate")
  )
  # fit an equivalent forecaster
  equivalent <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead + latency)
  equiv_fit <- slm_fit(equivalent, data = real_x)
  # adjusting the ahead should do the same thing as directly adjusting the ahead
  expect_equal(
    fit5$fit$fit$fit$coefficients,
    equiv_fit$fit$fit$fit$coefficients
  )

  # should have four predictors, including the intercept
  expect_equal(length(fit5$fit$fit$fit$coefficients), 6)
})

test_that("epi_adjust_latency fixed_* work", {})
# todo test variants on the columns for which this is applied
# todo need to have both on columns 1, and 2

test_that("epi_adjust_latency works correctly when there's gaps in the timeseries", {})

test_that("epi_adjust_latency extend_ahead uses the same adjustment when predicting on new data after being baked", {})

test_that("epi_adjust_latency works for other time types", {})

# todo check that epi_adjust_latency errors for nonsense `as_of`'s
