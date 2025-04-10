test_that("Yeo-Johnson transformation inverts correctly", {
  # Vectorized x and scalar lambda work
  lambdas <- seq(-5, 5, 0.1)
  x <- seq(-10, 10, 0.1)
  expect_true(
    map_lgl(lambdas, \(lambda) sum(abs(yj_inverse(yj_transform(x, lambda), lambda) - x)) < 1e-5) %>%
      all()
  )
  # Note that the special lambda values of 0 and 2 are covered by the tests
  # below.
  # Vectorized x and lambda both work
  x <- seq(-5, 5, 0.1)
  expect_true(
    sum(abs(yj_inverse(yj_transform(x, lambdas), lambdas) - x)) < 1e-5
  )

  # also works on quantile distributions
  x <- quantile_pred(matrix(c(-5, 1, 3, 0, 0.1, 0.5), nrow = 2, byrow = TRUE), c(0.01, 0.5, 0.7))
  x_back <-
    map(
      lambdas,
      \(lambda) mean(abs(yj_inverse(yj_transform(x, lambda), lambda) - x)) < 1e-5
    )
  expect_true(all(unlist(x_back)))

  # Get coverage on yj_input_type_management
  # Breaks on bad length of lambda
  expect_snapshot(error = TRUE,
    yj_input_type_management(x, c(1, 2, 3))
  )
  expect_snapshot(error = TRUE,
    yj_input_type_management(list(1, 2), c(1, 2, 3))
  )
  expect_true(
    identical(
      yj_input_type_management(list(1, 2, 3), c(1, 2, 3)),
      list(c(1, 2, 3), c(1, 2, 3))
    )
  )
})

test_that("Yeo-Johnson steps and layers invert each other", {
  jhu <- epidatasets::cases_deaths_subset %>%
    filter(time_value > "2021-01-01", geo_value %in% c("ca", "ny")) %>%
    select(geo_value, time_value, cases)
  filtered_data <- jhu

  # Get some yj_param values
  r <- epi_recipe(filtered_data) %>%
    step_epi_YeoJohnson(cases) %>%
    step_epi_lag(cases, lag = 0) %>%
    step_epi_ahead(cases, ahead = 0, role = "outcome") %>%
    step_epi_naomit()
  tr <- r %>% prep(filtered_data)

  # Check general yj_param values tibble structure
  expect_true(".yj_param_cases" %in% names(tr$steps[[1]]$yj_params))
  expect_true(is.numeric(tr$steps[[1]]$yj_params$.yj_param_cases))

  # Make sure that the inverse transformation works
  f <- frosting() %>%
    layer_predict() %>%
    layer_epi_YeoJohnson()
  wf <- epi_workflow(r, linear_reg()) %>%
    fit(filtered_data) %>%
    add_frosting(f)
  out1 <- filtered_data %>%
    dplyr::slice_max(time_value, by = geo_value)
  out2 <- forecast(wf) %>% rename(cases = .pred)
  expect_equal(out1, out2)

  # Make sure it works when there are multiple predictors
  jhu_multi <- epidatasets::covid_case_death_rates_extended %>%
    filter(time_value > "2021-01-01", geo_value %in% c("ca", "ny")) %>%
    select(geo_value, time_value, case_rate, death_rate)
  filtered_data <- jhu_multi
  r <- epi_recipe(filtered_data) %>%
    step_epi_YeoJohnson(case_rate, death_rate) %>%
    step_epi_lag(case_rate, death_rate, lag = 0) %>%
    step_epi_ahead(case_rate, ahead = 0, role = "outcome") %>%
    step_epi_naomit()
  tr <- r %>% prep(filtered_data)

  # Check general yj_param values tibble structure
  expect_true(".yj_param_case_rate" %in% names(tr$steps[[1]]$yj_params))
  expect_true(".yj_param_death_rate" %in% names(tr$steps[[1]]$yj_params))
  expect_true(is.numeric(tr$steps[[1]]$yj_params$.yj_param_case_rate))
  expect_true(is.numeric(tr$steps[[1]]$yj_params$.yj_param_death_rate))

  # Make sure that the inverse transformation works
  f <- frosting() %>%
    layer_predict() %>%
    layer_epi_YeoJohnson()
  wf <- epi_workflow(r, linear_reg()) %>%
    fit(filtered_data) %>%
    add_frosting(f)
  out1 <- filtered_data %>%
    select(-death_rate) %>%
    dplyr::slice_max(time_value, by = geo_value)
  out2 <- forecast(wf) %>% rename(case_rate = .pred)
  expect_equal(out1, out2)
})

test_that("Yeo-Johnson layers work on quantiles", {
  jhu <- epidatasets::cases_deaths_subset %>%
    filter(time_value > "2021-01-01", geo_value %in% c("ca", "ny")) %>%
    select(geo_value, time_value, cases)
  filtered_data <- jhu

  r <- epi_recipe(filtered_data) %>%
    step_epi_YeoJohnson(cases) %>%
    step_epi_lag(cases, lag = 0) %>%
    step_epi_ahead(cases, ahead = 0, role = "outcome") %>%
    step_epi_naomit()

  f <- frosting() %>%
    layer_predict() %>%
    layer_residual_quantiles() %>%
    layer_epi_YeoJohnson()
  wf <- epi_workflow(r, linear_reg()) %>%
    fit(filtered_data) %>%
    add_frosting(f)
  out1 <- filtered_data %>%
    dplyr::slice_max(time_value, by = geo_value) %>%
    rename(.pred = cases) %>%
    tidyr::expand_grid(.pred_distn_quantile_level = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)) %>%
    mutate(.pred_distn_value = .pred) %>%
    select(geo_value, time_value, .pred, .pred_distn_value, .pred_distn_quantile_level)
  out2 <- forecast(wf) %>% pivot_quantiles_longer(.pred_distn) %>% as_tibble()
  expect_equal(out1, out2)
})

test_that("Yeo-Johnson steps and layers invert each other when other_keys are present", {
  # Small synthetic grad_employ_dataset version.
  # fmt: skip
  filtered_data <- tribble(
    ~geo_value, ~age_group, ~edu_qual, ~time_value, ~med_income_2y,
    "ca", "25-34", "bachelor", 2017, 50000,
    "ca", "25-34", "bachelor", 2018, 50500,
    "ca", "25-34", "bachelor", 2019, 51000,
    "ca", "25-34", "bachelor", 2020, 51500,
    "ca", "25-34", "bachelor", 2021, 52000,
    "ca", "25-34", "bachelor", 2022, 52500,
    "ca", "35-1000", "bachelor", 2017, 3e10,
    "ca", "35-1000", "bachelor", 2018, 3e10 + 10,
    "ca", "35-1000", "bachelor", 2019, 3e10 + 20,
    "ca", "35-1000", "bachelor", 2020, 3e10 + 30,
    "ca", "35-1000", "bachelor", 2021, 3e10 + 40,
    "ca", "35-1000", "bachelor", 2022, 3e10 + 50,
    "ca", "25-34", "master", 2017, 2 * 50000,
    "ca", "25-34", "master", 2018, 2 * 50500,
    "ca", "25-34", "master", 2019, 2 * 51000,
    "ca", "25-34", "master", 2020, 2 * 51500,
    "ca", "25-34", "master", 2021, 2 * 52000,
    "ca", "25-34", "master", 2022, 2 * 52500,
    "ca", "35-1000", "master", 2017, 2 * 3e10,
    "ca", "35-1000", "master", 2018, 2 * (3e10 + 10),
    "ca", "35-1000", "master", 2019, 2 * (3e10 + 20),
    "ca", "35-1000", "master", 2020, 2 * (3e10 + 30),
    "ca", "35-1000", "master", 2021, 2 * (3e10 + 40),
    "ca", "35-1000", "master", 2022, 2 * (3e10 + 50)
  ) %>% as_epi_df(other_keys = c("age_group", "edu_qual"))

  # Get some yj_param values
  r <- epi_recipe(filtered_data) %>%
    step_epi_YeoJohnson(med_income_2y) %>%
    step_epi_lag(med_income_2y, lag = 0) %>%
    step_epi_ahead(med_income_2y, ahead = 0, role = "outcome") %>%
    step_epi_naomit()
  tr <- r %>% prep(filtered_data)
  expect_true(".yj_param_med_income_2y" %in% names(tr$steps[[1]]$yj_params))
  expect_true("geo_value" %in% names(tr$steps[[1]]$yj_params))
  expect_true("age_group" %in% names(tr$steps[[1]]$yj_params))
  expect_true("edu_qual" %in% names(tr$steps[[1]]$yj_params))
  expect_true(is.numeric(tr$steps[[1]]$yj_params$.yj_param_med_income_2y))

  # Make sure that the inverse transformation works
  f <- frosting() %>%
    layer_predict() %>%
    layer_residual_quantiles() %>%
    layer_epi_YeoJohnson(.pred)
  wf <- epi_workflow(r, linear_reg()) %>%
    fit(filtered_data) %>%
    add_frosting(f)
  out1 <- filtered_data %>%
    dplyr::slice_max(time_value, by = geo_value) %>%
    select(geo_value, age_group, time_value, med_income_2y) %>%
    arrange(geo_value, age_group, time_value)
  out2 <- forecast(wf) %>%
    rename(med_income_2y = .pred) %>%
    select(geo_value, age_group, time_value, med_income_2y) %>%
    arrange(geo_value, age_group, time_value)
  expect_equal(out1, out2)
})
