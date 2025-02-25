library(dplyr)
# Test ideas that were dropped:
# - "epi_adjust_latency works correctly when there's gaps in the timeseries"
# - "epi_adjust_latency extend_ahead uses the same adjustment when predicting on new data after being baked"
# - "`step_adjust_latency` only allows one instance of itself"
# - "data with epi_df shorn off works"

x <- tibble(
  geo_value = rep("place", 200),
  time_value = as.Date("2021-01-01") + 0:199,
  case_rate = sqrt(1:200) + atan(0.1 * 1:200) + sin(5 * 1:200) + 1,
  death_rate = atan(0.1 * 1:200) + cos(5 * 1:200) + 1
) %>%
  as_epi_df(as_of = as.POSIXct("2024-09-17"))
max_time <- max(x$time_value)
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


# making a toy dataset with lag between geo_values
x_lagged <- x
x_lagged$time_value <- x$time_value - 1
x_lagged$geo_value <- "other"
x_lagged <- add_row(x, x_lagged)
x_lagged
attributes(x_lagged)$metadata$as_of <- testing_as_of

test_that("epi_adjust_latency correctly extends the lags", {
  expect_warning(epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_adjust_latency(method = "extend_lags"))

  r1 <- epi_recipe(x) %>%
    step_adjust_latency(method = "extend_lags") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  # directly checking the shifts
  baked_x <- r1 %>%
    prep(real_x) %>%
    bake(real_x)
  # map each column to its last non-NA value
  last_dates <- baked_x %>%
    tidyr::pivot_longer(cols = contains("rate"), values_drop_na = TRUE) %>%
    group_by(name) %>%
    summarise(last_date = max(time_value)) %>%
    arrange(desc(last_date))
  expect_equal(
    last_dates,
    tribble(
      ~name, ~last_date,
      "lag_16_death_rate", max_time + 16,
      "lag_11_death_rate", max_time + 11,
      "lag_10_case_rate", max_time + 10,
      "lag_6_case_rate", max_time + 6,
      "lag_5_death_rate", max_time + 5,
      "case_rate", max_time,
      "death_rate", max_time,
      "ahead_7_death_rate", max_time - 7,
    )
  )

  # the as_of on x is today's date, which is >970 days in the future
  # also, there's no data >970 days in the past, so it gets an error trying to
  # fit on no data
  expect_error(
    expect_warning(
      expect_warning(
        fit1 <- slm_fit(r1, data = x),
        class = "epipredict__prep.step_latency__very_large_latency"
      ),
      class = "epipredict__prep.step_latency__very_large_latency"
    ),
    class = "simpleError"
  )

  # now trying with the as_of a reasonable distance in the future
  fit1 <- slm_fit(r1, data = real_x)
  expect_equal(
    names(fit1$pre$mold$predictors),
    c(
      "lag_5_death_rate", "lag_11_death_rate", "lag_16_death_rate",
      "lag_6_case_rate", "lag_10_case_rate"
    )
  )
  latest <- get_test_data(r1, real_x)
  pred <- predict(fit1, latest)
  point_pred <- pred %>% filter(!is.na(.pred))
  expect_equal(nrow(point_pred), 1)
  expect_equal(point_pred$time_value, as.Date(testing_as_of))

  expect_equal(
    names(fit1$pre$mold$outcomes),
    glue::glue("ahead_{ahead}_death_rate")
  )
  latest <- get_test_data(r1, x)
  pred1 <- predict(fit1, latest)
  actual_solutions <- pred1 %>% filter(!is.na(.pred))
  expect_equal(actual_solutions$time_value, testing_as_of)

  # should have four predictors, including the intercept
  expect_equal(length(fit1$fit$fit$fit$coefficients), 6)

  # result should be equivalent to just immediately doing the adjusted lags by
  # hand
  hand_adjusted <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(5, 11, 16)) %>%
    step_epi_lag(case_rate, lag = c(6, 10)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  fit_hand_adj <- slm_fit(hand_adjusted, data = real_x)
  expect_equal(
    fit1$fit$fit$fit$coefficients,
    fit_hand_adj$fit$fit$fit$coefficients
  )
})

test_that("epi_adjust_latency correctly extends the ahead", {
  r2 <- epi_recipe(x) %>%
    step_adjust_latency(method = "extend_ahead") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  # the as_of on x is today's date, which is >970 days in the future
  # also, there's no data >970 days in the past, so it gets an error trying to
  # fit on no data
  expect_error(expect_warning(fit5 <- slm_fit(r2), class = "epipredict__prep.step_latency__very_large_latency"), class = "simpleError")
  # real date example
  fit2 <- slm_fit(r2, data = real_x)
  expect_equal(
    names(fit2$pre$mold$predictors),
    c(
      "lag_0_death_rate", "lag_6_death_rate", "lag_11_death_rate",
      "lag_1_case_rate", "lag_5_case_rate"
    )
  )
  latest <- get_test_data(r2, real_x)
  pred2 <- predict(fit2, latest)
  point_pred2 <- pred2 %>% filter(!is.na(.pred))
  # max time is still the forecast date
  expect_equal(point_pred2$time_value, as.Date(max_time))
  # target column renamed
  expect_equal(
    names(fit2$pre$mold$outcomes),
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
    fit2$fit$fit$fit$coefficients,
    equiv_fit$fit$fit$fit$coefficients
  )

  # should have four predictors, including the intercept
  expect_equal(length(fit2$fit$fit$fit$coefficients), 6)
})

test_that("epi_adjust_latency correctly locfs", {
  r1 <- epi_recipe(x) %>%
    step_adjust_latency(method = "locf") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)

  # directly checking the shifts
  baked_x <- r1 %>%
    prep(real_x) %>%
    bake(real_x)
  # map each column to its last non-NA value
  last_dates <- baked_x %>%
    tidyr::pivot_longer(cols = contains("rate"), values_drop_na = TRUE) %>%
    group_by(name) %>%
    summarise(last_date = max(time_value)) %>%
    arrange(desc(last_date))
  expect_equal(
    last_dates,
    tribble(
      ~name, ~last_date,
      "lag_11_death_rate", max_time + 16,
      "lag_6_death_rate", max_time + 11,
      "lag_5_case_rate", max_time + 10,
      "lag_1_case_rate", max_time + 6,
      "case_rate", max_time + 5,
      "death_rate", max_time + 5,
      "lag_0_death_rate", max_time + 5,
      "ahead_7_death_rate", max_time - 2,
    )
  )
  # we expect a 5-fold repetition of the last values found in the original
  # epi_df
  last_real <- real_x %>%
    group_by(geo_value) %>%
    arrange(time_value) %>%
    slice_tail() %>%
    ungroup() %>%
    select(case_rate, death_rate) %>%
    tidyr::uncount(5)
  # pulling just the region between the last day and the prediction day
  filled_values <-
    baked_x %>%
    filter(
      time_value > max(real_x$time_value),
      time_value <= attributes(real_x)$metadata$as_of
    ) %>%
    ungroup() %>%
    select(case_rate, death_rate)
  expect_equal(last_real, filled_values)
})

test_that("epi_adjust_latency extends multiple aheads", {
  aheads <- 1:3
  r3 <- epi_recipe(x) %>%
    step_adjust_latency(method = "extend_ahead") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = aheads)
  fitter <- smooth_quantile_reg(
    quantile_levels = 0.5,
    outcome_locations = aheads,
    degree = 1L
  )
  epi_wf <- epi_workflow(r3, fitter)
  # the as_of on x is today's date, which is >970 days in the future
  # also, there's no data >970 days in the past, so it gets an error trying to
  # fit on no data
  expect_error(
    expect_warning(
      fit3 <- fit(epi_wf, data = x),
      class = "epipredict__prep.step_latency__very_large_latency"
    ),
    class = "simpleError"
  )
  # real date example
  fit3 <- fit(epi_wf, data = real_x)
  expect_equal(
    names(fit3$pre$mold$outcomes),
    c(
      "ahead_6_death_rate", "ahead_7_death_rate", "ahead_8_death_rate"
    )
  )
  expect_equal(
    names(fit3$pre$mold$predictors),
    c(
      "lag_0_death_rate", "lag_6_death_rate", "lag_11_death_rate",
      "lag_1_case_rate", "lag_5_case_rate"
    )
  )
  latest <- get_test_data(r3, real_x)
  pred3 <- predict(fit3, latest)
  point_pred <- pred3 %>%
    unnest(.pred) %>%
    filter(!is.na(distn))
  # max time is still the forecast date
  expect_equal(
    point_pred$time_value,
    rep(as.Date(max_time), 3)
  )
  # fit an equivalent forecaster
  equivalent <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead + latency)
  equiv_fit <- fit(epi_wf, data = real_x)
  # adjusting the ahead should do the same thing as directly adjusting the ahead
  expect_equal(
    fit3$fit$fit$fit$rqfit,
    equiv_fit$fit$fit$fit$rqfit
  )

  # should have four predictors, including the intercept
  expect_equal(length(fit3$fit$fit$fit$rqfit$coefficients), 6)
})

test_that("epi_adjust_latency fixed_forecast_date works", {
  r4 <- epi_recipe(x) %>%
    step_adjust_latency(method = "extend_lags", fixed_forecast_date = max_time + 14) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  baked_x <- r4 %>%
    prep(real_x) %>%
    bake(real_x)
  # map each column to its last non-NA value
  last_dates <- baked_x %>%
    tidyr::pivot_longer(cols = contains("rate"), values_drop_na = TRUE) %>%
    group_by(name) %>%
    summarise(last_date = max(time_value)) %>%
    arrange(desc(last_date))
  expect_equal(
    last_dates,
    tribble(
      ~name, ~last_date,
      "lag_25_death_rate", max_time + 25,
      "lag_20_death_rate", max_time + 20,
      "lag_19_case_rate", max_time + 19,
      "lag_15_case_rate", max_time + 15,
      "lag_14_death_rate", max_time + 14,
      "case_rate", max_time,
      "death_rate", max_time,
      "ahead_7_death_rate", max_time - 7,
    )
  )
})

test_that("epi_adjust_latency fixed_latency works", {
  r4.1 <- epi_recipe(x) %>%
    step_adjust_latency(method = "extend_lags", fixed_latency = 2) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  baked_x <- r4.1 %>%
    prep(real_x) %>%
    bake(real_x)
  # map each column to its last non-NA value
  last_dates <- baked_x %>%
    tidyr::pivot_longer(cols = contains("rate"), values_drop_na = TRUE) %>%
    group_by(name) %>%
    summarise(last_date = max(time_value)) %>%
    arrange(desc(last_date))
  expect_equal(
    last_dates,
    tribble(
      ~name, ~last_date,
      "lag_13_death_rate", max_time + 13,
      "lag_8_death_rate", max_time + 8,
      "lag_7_case_rate", max_time + 7,
      "lag_3_case_rate", max_time + 3,
      "lag_2_death_rate", max_time + 2,
      "case_rate", max_time,
      "death_rate", max_time,
      "ahead_7_death_rate", max_time - 7,
    )
  )
})


# todo test variants on the columns for which this is applied
# todo need to have both on columns 1, and 2



# test_that("epi_adjust_latency works for other time types", {})

test_that("epi_adjust_latency warns there's steps before it", {
  expect_warning(
    r5 <- epi_recipe(x) %>%
      step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
      step_adjust_latency(method = "extend_lags"),
    regexp = "extend_lags"
  )
  expect_warning(
    r5 <- epi_recipe(x) %>%
      step_epi_ahead(death_rate, ahead = ahead) %>%
      step_adjust_latency(method = "extend_ahead"),
    regexp = "extend_ahead"
  )
})

# TODO check that epi_adjust_latency errors for nonsense `as_of`'s


# TODO make sure that `epi_keys_checked` works correctly for extra epi_keys

test_that("epi_adjust_latency correctly extends the lags when there are different delays per-geo", {
  r5 <- epi_recipe(x_lagged) %>%
    step_adjust_latency(method = "extend_lags") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  # now trying with the as_of a reasonable distance in the future
  fit5 <- slm_fit(r5, data = x_lagged)
  expect_equal(
    names(fit5$pre$mold$predictors),
    c(
      "lag_6_death_rate", "lag_12_death_rate", "lag_17_death_rate",
      "lag_7_case_rate", "lag_11_case_rate"
    )
  )
  latest <- get_test_data(r5, x_lagged)
  pred <- predict(fit5, latest)
  point_pred <- pred %>% filter(!is.na(.pred))
  expect_equal(nrow(point_pred), 1)
  expect_equal(point_pred$time_value, as.Date(testing_as_of) + 1)

  expect_equal(
    names(fit5$pre$mold$outcomes),
    glue::glue("ahead_{ahead}_death_rate")
  )

  # should have four predictors, including the intercept
  expect_equal(length(fit5$fit$fit$fit$coefficients), 6)

  # result should be equivalent to just immediately doing the adjusted lags by
  # hand
  hand_adjusted <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(6, 12, 17)) %>%
    step_epi_lag(case_rate, lag = c(7, 11)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  fit_hand_adj <- slm_fit(hand_adjusted, data = real_x)
  expect_equal(
    fit5$fit$fit$fit$coefficients,
    fit_hand_adj$fit$fit$fit$coefficients
  )
})

test_that("epi_adjust_latency correctly extends the ahead when there are different delays per-geo", {
  r5 <- epi_recipe(x_lagged) %>%
    step_adjust_latency(method = "extend_ahead") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)

  fit5 <- slm_fit(r5, data = x_lagged)
  expect_equal(
    names(fit5$pre$mold$predictors),
    c(
      "lag_0_death_rate", "lag_6_death_rate", "lag_11_death_rate",
      "lag_1_case_rate", "lag_5_case_rate"
    )
  )
  latest <- get_test_data(r5, x_lagged)
  pred <- predict(fit5, latest)
  point_pred <- pred %>% filter(!is.na(.pred))
  expect_equal(nrow(point_pred), 1)
  expect_equal(point_pred$time_value, as.Date(max_time))
  joint_latency <- latency + 1
  expect_equal(
    names(fit5$pre$mold$outcomes),
    glue::glue("ahead_{ahead+joint_latency}_death_rate")
  )
  actual_solutions <- pred %>% filter(!is.na(.pred))
  expect_equal(actual_solutions$time_value, as.Date(max_time))

  # should have four predictors, including the intercept
  expect_equal(length(fit5$fit$fit$fit$coefficients), 6)

  # result should be equivalent to just immediately doing the adjusted lags by
  # hand
  hand_adjusted <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead + joint_latency)

  fit_hand_adj <- slm_fit(hand_adjusted, data = real_x)
  expect_equal(
    fit5$fit$fit$fit$coefficients,
    fit_hand_adj$fit$fit$fit$coefficients
  )
})

test_that("`step_adjust_latency` only uses the columns specified in the `...`", {
  r5 <- epi_recipe(x) %>%
    step_adjust_latency(case_rate, method = "extend_lags") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)

  fit5 <- slm_fit(r5, data = real_x)
  expect_equal(names(fit5$fit$fit$fit$coefficients), c("(Intercept)", "lag_0_death_rate", "lag_6_death_rate", "lag_11_death_rate", "lag_6_case_rate", "lag_10_case_rate"))

  r51 <- epi_recipe(x) %>%
    step_adjust_latency(case_rate, method = "locf") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)

  baked_x <- r51 %>%
    prep(real_x) %>%
    bake(real_x)
  # map each column to its last non-NA value
  last_dates <- baked_x %>%
    tidyr::pivot_longer(cols = contains("rate"), values_drop_na = TRUE) %>%
    group_by(name) %>%
    summarise(last_date = max(time_value)) %>%
    arrange(desc(last_date)) %>%
    mutate(locf_date = last_date - latency)
  # iterate over all columns and make sure the latent time period has the exact same values (so the variance is zero)
  for (ii in seq(nrow(last_dates))) {
    baked_var <- baked_x %>%
      filter(last_dates[[ii, "locf_date"]] <= time_value, time_value <= last_dates[[ii, "last_date"]]) %>%
      pull(last_dates[[ii, "name"]]) %>%
      var()
    if (grepl("case_rate", last_dates[[ii, "name"]])) {
      expect_equal(baked_var, 0)
    } else {
      expect_true(baked_var > 0)
    }
  }
})

test_that("printing step_adjust_latency results in expected output", {
  r5 <- epi_recipe(x) %>%
    step_adjust_latency(case_rate, method = "extend_lags") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  expect_snapshot(r5)
  expect_snapshot(prep(r5, real_x))
  r6 <- epi_recipe(covid_case_death_rates) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_adjust_latency(method = "extend_ahead") %>%
    step_epi_ahead(death_rate, ahead = 7)
  expect_snapshot(r6)
  expect_snapshot(prep(r6, covid_case_death_rates))
})

test_that("locf works as intended", {
  expect_warning(epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_adjust_latency(method = "locf"))

  r6 <- epi_recipe(x) %>%
    step_adjust_latency(method = "locf") %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)

  # directly checking the shifts
  baked_x <- r6 %>%
    prep(real_x) %>%
    bake(real_x)
  # map each column to its last non-NA value
  last_dates <- baked_x %>%
    tidyr::pivot_longer(cols = contains("rate"), values_drop_na = TRUE) %>%
    group_by(name) %>%
    summarise(last_date = max(time_value)) %>%
    arrange(desc(last_date)) %>%
    mutate(locf_date = last_date - latency)
  # iterate over all columns and make sure the latent time period has the exact same values
  for (ii in seq(nrow(last_dates))) {
    baked_x %>%
      filter(last_dates[[ii, "locf_date"]] <= time_value, time_value <= last_dates[[ii, "last_date"]]) %>%
      pull(last_dates[[ii, "name"]]) %>%
      var() %>%
      expect_equal(0)
  }

  # the as_of on x is today's date, which is >970 days in the future
  # also, there's no data >970 days in the past, so it gets an error trying to
  # fit on no data
  expect_warning(fit6 <- slm_fit(r6, data = x),
    class = "epipredict__prep.step_latency__very_large_latency"
  )

  # now trying with the as_of a reasonable distance in the future
  fit6 <- slm_fit(r6, data = real_x)
  expect_equal(
    names(fit6$pre$mold$predictors),
    c(
      "lag_0_death_rate", "lag_6_death_rate", "lag_11_death_rate",
      "lag_1_case_rate", "lag_5_case_rate"
    )
  )
  latest <- get_test_data(r6, real_x)
  pred <- predict(fit6, latest)
  point_pred <- pred %>% filter(!is.na(.pred))
  expect_equal(max(point_pred$time_value), as.Date(testing_as_of))

  expect_equal(
    names(fit6$pre$mold$outcomes),
    glue::glue("ahead_{ahead}_death_rate")
  )
  latest <- get_test_data(r6, x)
  pred1 <- predict(fit6, latest)
  actual_solutions <- pred1 %>% filter(!is.na(.pred))
  expect_equal(max(actual_solutions$time_value), testing_as_of)

  # should have four predictors, including the intercept
  expect_equal(length(fit6$fit$fit$fit$coefficients), 6)

  # result should be equivalent to just immediately doing the adjusted lags by
  # hand
  #
  hand_adjusted <- epi_recipe(x) %>%
    step_epi_lag(death_rate, lag = c(0, 6, 11)) %>%
    step_epi_lag(case_rate, lag = c(1, 5)) %>%
    step_epi_ahead(death_rate, ahead = ahead)
  locf_x <- real_x %>% rbind(tibble(
    geo_value = rep("place", latency),
    time_value = max_time + 1:latency,
    case_rate = rep(real_x$case_rate[nrow(x)], latency),
    death_rate = rep(real_x$death_rate[nrow(x)], latency)
  ))
  fit_hand_adj <- slm_fit(hand_adjusted, data = locf_x)
  expect_equal(
    fit6$fit$fit$fit$coefficients,
    fit_hand_adj$fit$fit$fit$coefficients
  )
})
