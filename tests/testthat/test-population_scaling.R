## Preprocessing
test_that("Column names can be passed with and without the tidy way", {
  pop_data <- data.frame(
    states = c("ak", "al", "ar", "as", "az", "ca"),
    value = c(1000, 2000, 3000, 4000, 5000, 6000)
  )

  pop_data2 <- pop_data %>% dplyr::rename(geo_value = states)

  newdata <- covid_case_death_rates %>%
    filter(geo_value %in% c("ak", "al", "ar", "as", "az", "ca"))

  r1 <- epi_recipe(newdata) %>%
    step_population_scaling(
      case_rate, death_rate,
      df = pop_data,
      df_pop_col = "value",
      by = c("geo_value" = "states")
    )

  r2 <- epi_recipe(newdata) %>%
    step_population_scaling(
      case_rate, death_rate,
      df = pop_data2,
      df_pop_col = "value",
      by = "geo_value"
    )

  prep1 <- prep(r1, newdata)
  prep2 <- prep(r2, newdata)

  expect_equal(bake(prep1, newdata), bake(prep2, newdata))
})

test_that("Number of columns and column names returned correctly, Upper and lower cases handled properly ", {
  pop_data <- data.frame(
    states = c(rep("a", 5), rep("B", 5)),
    counties = c(
      "06059", "06061", "06067",
      "12111", "12113", "12117",
      "42101", "42103", "42105", "42111"
    ),
    value = 1000:1009
  )

  newdata <- tibble(
    geo_value = c(rep("a", 5), rep("b", 5)),
    county = c(
      "06059", "06061", "06067",
      "12111", "12113", "12117",
      "42101", "42103", "42105", "42111"
    ),
    time_value = rep(as.Date("2021-01-01") + 0:4, 2),
    case = 1:10,
    death = 1:10
  ) %>%
    epiprocess::as_epi_df()

  r <- epi_recipe(newdata) %>%
    step_population_scaling(c("case", "death"),
      df = pop_data,
      df_pop_col = "value", by = c("geo_value" = "states", "county" = "counties"),
      suffix = "_rate"
    )

  p <- prep(r, newdata)

  b <- bake(p, newdata)
  expect_equal(ncol(b), 7L)
  expect_true("case_rate" %in% colnames(b))
  expect_true("death_rate" %in% colnames(b))



  r <- epi_recipe(newdata) %>%
    step_population_scaling(
      df = pop_data,
      df_pop_col = "value",
      by = c("geo_value" = "states", "county" = "counties"),
      c("case", "death"),
      suffix = "_rate", # unused
      create_new = FALSE
    )

  expect_warning(p <- prep(r, newdata))

  expect_warning(b <- bake(p, newdata))
  expect_equal(ncol(b), 5L)
})

## Postprocessing
test_that("Postprocessing workflow works and values correct", {
  jhu <- cases_deaths_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, cases)

  pop_data <- data.frame(
    states = c("ca", "ny"),
    value = c(20000, 30000)
  )

  r <- epi_recipe(jhu) %>%
    step_population_scaling(cases,
      df = pop_data,
      df_pop_col = "value",
      by = c("geo_value" = "states"),
      role = "raw",
      suffix = "_scaled"
    ) %>%
    step_epi_lag(cases_scaled, lag = c(0, 7, 14)) %>%
    step_epi_ahead(cases_scaled, ahead = 7, role = "outcome") %>%
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = pop_data,
      by = c("geo_value" = "states"),
      df_pop_col = "value"
    )

  wf <- epi_workflow(r, parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  p <- forecast(wf)
  expect_equal(nrow(p), 2L)
  expect_equal(ncol(p), 4L)
  expect_equal(p$.pred_scaled, p$.pred * c(20000, 30000))

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = pop_data, rate_rescaling = 10000,
      by = c("geo_value" = "states"),
      df_pop_col = "value"
    )
  wf <- epi_workflow(r, parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)
  p <- forecast(wf)
  expect_equal(nrow(p), 2L)
  expect_equal(ncol(p), 4L)
  expect_equal(p$.pred_scaled, p$.pred * c(2, 3))
})

test_that("Postprocessing to get cases from case rate", {
  jhu <- covid_case_death_rates %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, case_rate)

  reverse_pop_data <- data.frame(
    states = c("ca", "ny"),
    value = c(1 / 20000, 1 / 30000)
  )

  r <- epi_recipe(jhu) %>%
    step_population_scaling(
      df = reverse_pop_data,
      df_pop_col = "value",
      by = c("geo_value" = "states"),
      case_rate, suffix = "_scaled"
    ) %>%
    step_epi_lag(case_rate_scaled, lag = c(0, 7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      by = c("geo_value" = "states"),
      df_pop_col = "value"
    )

  wf <- epi_workflow(r, parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  p <- forecast(wf)
  expect_equal(nrow(p), 2L)
  expect_equal(ncol(p), 4L)
  expect_equal(p$.pred_scaled, p$.pred * c(1 / 20000, 1 / 30000))
})


test_that("test joining by default columns", {
  jhu <- covid_case_death_rates %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, case_rate)

  reverse_pop_data <- data.frame(
    geo_value = c("ca", "ny"),
    values = c(1 / 20000, 1 / 30000)
  )

  r <- epi_recipe(jhu) %>%
    step_population_scaling(case_rate,
      df = reverse_pop_data,
      df_pop_col = "values",
      by = NULL,
      suffix = "_scaled"
    ) %>%
    step_epi_lag(case_rate_scaled, lag = c(0, 7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)

  prep <- prep(r, jhu)

  b <- bake(prep, jhu)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      by = NULL,
      df_pop_col = "values"
    )

  wf <- epi_workflow(
    r,
    parsnip::linear_reg()
  ) %>%
    fit(jhu) %>%
    add_frosting(f)

  latest <- get_test_data(
    recipe = r,
    x = covid_case_death_rates %>%
      dplyr::filter(
        time_value > "2021-11-01",
        geo_value %in% c("ca", "ny")
      ) %>%
      dplyr::select(geo_value, time_value, case_rate)
  )


  p <- predict(wf, latest)



  jhu <- covid_case_death_rates %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, case_rate)

  reverse_pop_data <- data.frame(
    geo_value = c("ca", "ny"),
    values = c(1 / 20000, 1 / 30000)
  )

  r <- epi_recipe(jhu) %>%
    step_population_scaling(case_rate,
      df = reverse_pop_data,
      df_pop_col = "values",
      by = NULL,
      suffix = "_scaled"
    ) %>%
    step_epi_lag(case_rate_scaled, lag = c(0, 7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)

  p <- prep(r, jhu)
  b <- bake(p, new_data = NULL)
  expect_named(
    b,
    c(
      "geo_value", "time_value", "case_rate", "case_rate_scaled",
      paste0("lag_", c(0, 7, 14), "_case_rate_scaled"),
      "ahead_7_case_rate_scaled"
    )
  )



  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      by = NULL,
      df_pop_col = "values"
    )

  wf <- epi_workflow(r, parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  fc <- forecast(wf)
  expect_named(fc, c("geo_value", "time_value", ".pred", ".pred_scaled"))
  expect_equal(fc$.pred_scaled, fc$.pred * c(1 / 20000, 1 / 30000))
})


test_that("test joining by default columns with less common keys/classes", {
  # Make a model spec that expects no predictor columns and outputs a fixed
  # (rate) prediction. Based on combining two linear inequalities.
  fixed_rate_prediction <- 2e-6
  model_spec <- quantile_reg(quantile_levels = 0.5, method = "fnc") %>%
    set_engine(
      "rq",
      R = matrix(c(1, -1), 2, 1), r = c(1, -1) * fixed_rate_prediction,
      eps = fixed_rate_prediction * 1e-6 # prevent early stop
    )

  # Here's the typical setup
  dat1 <- tibble::tibble(geo_value = 1:2, time_value = 1, y = c(3 * 5, 7 * 11)) %>%
    as_epi_df()
  pop1 <- tibble::tibble(geo_value = 1:2, population = c(5e6, 11e6))
  ewf1 <- epi_workflow(
    epi_recipe(dat1) %>%
      step_population_scaling(y, df = pop1, df_pop_col = "population") %>%
      step_epi_ahead(y_scaled, ahead = 0),
    model_spec,
    frosting() %>%
      layer_predict() %>%
      layer_population_scaling(.pred, df = pop1, df_pop_col = "population", create_new = FALSE)
  )
  expect_equal(
    extract_recipe(ewf1, estimated = FALSE) %>%
      prep(dat1) %>%
      bake(new_data = NULL),
    dat1 %>%
      mutate(y_scaled = c(3e-6, 7e-6), ahead_0_y_scaled = y_scaled)
  )
  expect_equal(
    forecast(fit(ewf1, dat1)) %>%
      pivot_quantiles_wider(.pred),
    dat1 %>%
      select(!"y") %>%
      as_tibble() %>%
      mutate(`0.5` = c(2 * 5, 2 * 11))
  )

  # with age_group breakdown instead:
  dat2 <- dat1 %>%
    as_tibble() %>%
    mutate(age_group = geo_value, geo_value = 1) %>%
    as_epi_df(other_keys = "age_group")
  pop2 <- pop1 %>%
    mutate(age_group = geo_value, geo_value = 1)
  ewf2 <- epi_workflow(
    epi_recipe(dat2) %>%
      step_population_scaling(y, df = pop2, df_pop_col = "population") %>%
      step_epi_ahead(y_scaled, ahead = 0),
    model_spec,
    frosting() %>%
      layer_predict() %>%
      layer_population_scaling(.pred, df = pop2, df_pop_col = "population", create_new = FALSE)
  )
  expect_equal(
    extract_recipe(ewf2, estimated = FALSE) %>%
      prep(dat2) %>%
      bake(new_data = NULL),
    dat2 %>%
      mutate(y_scaled = c(3e-6, 7e-6), ahead_0_y_scaled = y_scaled)
  )
  expect_equal(
    forecast(fit(ewf2, dat2)) %>%
      pivot_quantiles_wider(.pred),
    dat2 %>%
      select(!"y") %>%
      as_tibble() %>%
      mutate(`0.5` = c(2 * 5, 2 * 11))
  )

  # with time_value breakdown instead:
  dat3 <- dat1 %>%
    as_tibble() %>%
    mutate(time_value = geo_value, geo_value = 1) %>%
    as_epi_df()
  pop3 <- pop1 %>%
    mutate(time_value = geo_value, geo_value = 1)
  ewf3 <- epi_workflow(
    epi_recipe(dat3) %>%
      step_population_scaling(y, df = pop3, df_pop_col = "population") %>%
      step_epi_ahead(y_scaled, ahead = 0),
    model_spec,
    frosting() %>%
      layer_predict() %>%
      layer_population_scaling(.pred, df = pop3, df_pop_col = "population", create_new = FALSE)
  )
  expect_equal(
    extract_recipe(ewf3, estimated = FALSE) %>%
      prep(dat3) %>%
      bake(new_data = NULL),
    dat3 %>%
      mutate(y_scaled = c(3e-6, 7e-6), ahead_0_y_scaled = y_scaled)
  )
  expect_equal(
    forecast(fit(ewf3, dat3)) %>%
      pivot_quantiles_wider(.pred),
    # slightly edited copy-pasta due to test time selection:
    dat3 %>%
      select(!"y") %>%
      as_tibble() %>%
      slice_max(by = geo_value, time_value) %>%
      mutate(`0.5` = 2 * 11)
  )

  # TODO non-`epi_df` scaling?

  # TODO multikey scaling?

})


test_that("expect error if `by` selector does not match", {
  jhu <- covid_case_death_rates %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, case_rate)

  reverse_pop_data <- data.frame(
    geo_value = c("ca", "ny"),
    values = c(1 / 20000, 1 / 30000)
  )

  r <- epi_recipe(jhu) %>%
    step_population_scaling(case_rate,
      df = reverse_pop_data,
      df_pop_col = "values",
      by = c("a" = "b"),
      suffix = "_scaled"
    ) %>%
    step_epi_lag(case_rate_scaled, lag = c(0, 7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      by = NULL,
      df_pop_col = "values"
    )

  expect_snapshot(
    error = TRUE,
    wf <- epi_workflow(r, parsnip::linear_reg()) %>%
      fit(jhu) %>%
      add_frosting(f)
  )

  r <- epi_recipe(jhu) %>%
    step_population_scaling(case_rate,
      df = reverse_pop_data,
      df_pop_col = "values",
      by = c("geo_value" = "geo_value"),
      suffix = "_scaled"
    ) %>%
    step_epi_lag(case_rate_scaled, lag = c(0, 7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      by = c("nothere" = "nope"),
      df_pop_col = "values"
    )

  wf <- epi_workflow(r, parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  expect_snapshot(error = TRUE, forecast(wf))
})


test_that("Rate rescaling behaves as expected", {
  x <- tibble(
    geo_value = rep("place", 50),
    time_value = as.Date("2021-01-01") + 0:49,
    case_rate = rep(0.0005, 50),
    cases = rep(5000, 50)
  ) %>%
    as_epi_df()

  reverse_pop_data <- data.frame(
    states = c("place"),
    value = c(1 / 1000)
  )

  r <- epi_recipe(x) %>%
    step_population_scaling(
      df = reverse_pop_data,
      df_pop_col = "value",
      rate_rescaling = 100, # cases per 100
      by = c("geo_value" = "states"),
      case_rate, suffix = "_scaled"
    )

  expect_equal(
    unique(bake(prep(r, x), x)$case_rate_scaled),
    0.0005 * 100 / (1 / 1000)
  ) # done testing step_*

  f <- frosting() %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      rate_rescaling = 100, # revert back to case rate per 100
      by = c("geo_value" = "states"),
      df_pop_col = "value"
    )

  x <- tibble(
    geo_value = rep("place", 50),
    time_value = as.Date("2021-01-01") + 0:49,
    case_rate = rep(0.0005, 50)
  ) %>%
    as_epi_df()

  r <- epi_recipe(x) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>% # cases
    step_epi_ahead(case_rate, ahead = 7, role = "outcome") %>% # cases
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      rate_rescaling = 100, # revert back to case rate per 100
      by = c("geo_value" = "states"),
      df_pop_col = "value"
    )

  wf <- epi_workflow(r, parsnip::linear_reg()) %>%
    fit(x) %>%
    add_frosting(f)

  # suppress warning: prediction from a rank-deficient fit may be misleading
  suppressWarnings(expect_equal(
    unique(forecast(wf)$.pred) * (1 / 1000) / 100,
    unique(forecast(wf)$.pred_scaled)
  ))
})

test_that("Extra Columns are ignored", {
  x <- tibble::tibble(
    geo_value = rep("place", 50),
    time_value = as.Date("2021-01-01") + 0:49,
    case_rate = rep(0.0005, 50),
    cases = rep(5000, 50)
  ) %>%
    as_epi_df()

  reverse_pop_data <- data.frame(
    states = c("place"),
    value = c(1 / 1000),
    extra_col = c("full name")
  )
  recip <- epi_recipe(x) %>%
    step_population_scaling(
      df = reverse_pop_data,
      df_pop_col = "value",
      rate_rescaling = 100, # cases per 100
      by = c("geo_value" = "states"),
      case_rate, suffix = "_scaled"
    ) %>%
    step_epi_lag(case_rate_scaled, lag = c(0, 7, 14)) %>% # cases
    step_epi_ahead(case_rate, ahead = 7, role = "outcome") %>% # cases
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
  expect_equal(ncol(bake(prep(recip, x), x)), 9)
  # done testing step_*

  frost <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred,
      df = reverse_pop_data,
      rate_rescaling = 100, # revert back to case rate per 100
      by = c("geo_value" = "states"),
      df_pop_col = "value"
    )

  wf <- epi_workflow(recip, parsnip::linear_reg()) %>%
    fit(x) %>%
    add_frosting(frost)
  # suppress warning: prediction from a rank-deficient fit may be misleading
  suppressWarnings(expect_equal(ncol(forecast(wf)), 4))
})
