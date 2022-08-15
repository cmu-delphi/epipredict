library(recipes)
library(parsnip)
library(workflows)

## Preprocessing
test_that("Column names can be passed with and without the tidy way", {
  pop_data = data.frame(states = c("ak","al","ar","as","az","ca"),
                        value = c(1000, 2000, 3000, 4000, 5000, 6000))

  newdata = case_death_rate_subset %>% filter(geo_value %in%  c("ak","al","ar","as","az","ca"))

  r1 <- epi_recipe(newdata) %>%
    step_population_scaling(c("case_rate", "death_rate"),
                            df = pop_data,
                            df_pop_col = "value", by = c("geo_value" = "states"))

  r2 <- epi_recipe(newdata) %>%
    step_population_scaling(case_rate, death_rate,
                            df = pop_data,
                            df_pop_col = "value", by = c("geo_value" = "states"))

  prep1 <- prep(r1, newdata)
  prep2 <- prep(r2, newdata)

  expect_equal(bake(prep1, newdata), bake(prep2, newdata))

})

test_that("Number of columns and column names returned correctly, Upper and lower cases handled properly ", {
  pop_data = data.frame(states = c(rep("a",5),  rep("B", 5)),
                        counties = c("06059","06061","06067",
                                   "12111","12113","12117",
                                   "42101","42103","42105", "42111"),
                        value = 1000:1009)

  newdata = tibble(geo_value = c(rep("a",5),  rep("b", 5)),
                   county = c("06059","06061","06067",
                              "12111","12113","12117",
                              "42101","42103","42105", "42111"),
                   time_value = rep(as.Date("2021-01-01") + 0:4, 2),
                   case = 1:10,
                   death = 1:10) %>%
    epiprocess::as_epi_df()

  r <-epi_recipe(newdata) %>%
    step_population_scaling(c("case", "death"),
                            df = pop_data,
                            df_pop_col = "value", by = c("geo_value" = "states", "county" = "counties"),
                            suffix = "_rate")

  prep <- prep(r, newdata)

  expect_silent(b <- bake(prep, newdata))
  expect_equal(ncol(b), 7L)
  expect_true("case_rate" %in% colnames(b))
  expect_true("death_rate" %in% colnames(b))



  r <-epi_recipe(newdata) %>%
    step_population_scaling(df = pop_data,
                            df_pop_col = "value",
                            by = c("geo_value" = "states", "county" = "counties"),
                            c("case", "death"),
                            suffix = "_rate", # unused
                            create_new = FALSE)

  prep <- prep(r, newdata)

  expect_message(b <- bake(prep, newdata))
  expect_equal(ncol(b), 5L)

})

## Postprocessing
test_that("Postprocessing workflow works and values correct", {
  jhu <- epiprocess::jhu_csse_daily_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, cases)

  pop_data = data.frame(states = c("ca", "ny"),
                        value = c(20000, 30000))

  r <- epi_recipe(jhu) %>%
    step_population_scaling(cases,
                            df = pop_data,
                            df_pop_col = "value",
                            by = c("geo_value" = "states"),
                            suffix = "_scaled") %>%
    step_epi_lag(cases_scaled, lag = c(7, 14)) %>%
    step_epi_ahead(cases_scaled, ahead = 7, role = "outcome") %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred, df = pop_data,
                             by =  c("geo_value" = "states"),
                             df_pop_col = "value")

  wf <- epi_workflow(r,
                     parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  latest <- get_test_data(recipe = r,
                x = epiprocess::jhu_csse_daily_subset %>%
                  dplyr::filter(time_value > "2021-11-01",
                                geo_value %in% c("ca", "ny")) %>%
                  dplyr::select(geo_value, time_value, cases))


  expect_silent(p <- predict(wf, latest))
  expect_equal(nrow(p), 2L)
  expect_equal(ncol(p), 4L)
  expect_equal(p$.pred_original, p$.pred * c(20000, 30000))
})

test_that("Postprocessing to get cases from case rate", {
  jhu <- case_death_rate_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, case_rate)

  reverse_pop_data = data.frame(states = c("ca", "ny"),
                        value = c(1/20000, 1/30000))

  r <- epi_recipe(jhu) %>%
    step_population_scaling(df = reverse_pop_data,
                            df_pop_col = "value",
                            by = c("geo_value" = "states"),
                            case_rate, suffix = "_scaled") %>%
    step_epi_lag(case_rate_scaled, lag = c(7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred, df = reverse_pop_data,
                             by =  c("geo_value" = "states"),
                             df_pop_col = "value")

  wf <- epi_workflow(r,
                     parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  latest <- get_test_data(recipe = r,
                          x = case_death_rate_subset %>%
                            dplyr::filter(time_value > "2021-11-01",
                                          geo_value %in% c("ca", "ny")) %>%
                            dplyr::select(geo_value, time_value, case_rate))


  expect_silent(p <- predict(wf, latest))
  expect_equal(nrow(p), 2L)
  expect_equal(ncol(p), 4L)
  expect_equal(p$.pred_original, p$.pred * c(1/20000, 1/30000))
})


test_that("test joining by default columns", {
  jhu <- case_death_rate_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, case_rate)

  reverse_pop_data = data.frame(geo_value = c("ca", "ny"),
                                values = c(1/20000, 1/30000))

  r <- epi_recipe(jhu) %>%
    step_population_scaling(case_rate,
                            df = reverse_pop_data,
                            df_pop_col = "values",
                            by = NULL,
                            suffix = "_scaled") %>%
    step_epi_lag(case_rate_scaled, lag = c(7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  prep <- prep(r, jhu)

  expect_message(b <- bake(prep, jhu))

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred, df = reverse_pop_data,
                             by =  NULL,
                             df_pop_col = "values")

  wf <- epi_workflow(r,
                     parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  latest <- get_test_data(recipe = r,
                          x = case_death_rate_subset %>%
                            dplyr::filter(time_value > "2021-11-01",
                                          geo_value %in% c("ca", "ny")) %>%
                            dplyr::select(geo_value, time_value, case_rate))


  expect_message(p <- predict(wf, latest))

})



test_that("expect error if `by` selector does not match", {
  jhu <- case_death_rate_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, case_rate)

  reverse_pop_data = data.frame(geo_value = c("ca", "ny"),
                                values = c(1/20000, 1/30000))

  r <- epi_recipe(jhu) %>%
    step_population_scaling(case_rate,
                            df = reverse_pop_data,
                            df_pop_col = "values",
                            by = c("a" = "b"),
                            suffix = "_scaled") %>%
    step_epi_lag(case_rate_scaled, lag = c(7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred, df = reverse_pop_data,
                             by =  NULL,
                             df_pop_col = "values")

  expect_error(wf <- epi_workflow(r,
                                    parsnip::linear_reg()) %>%
                   fit(jhu) %>%
                   add_frosting(f),
               "columns in `by` selectors of `step_population_scaling` must be present in data and match")

  r <- epi_recipe(jhu) %>%
    step_population_scaling(case_rate,
                            df = reverse_pop_data,
                            df_pop_col = "values",
                            by = c("geo_value" = "geo_value"),
                            suffix = "_scaled") %>%
    step_epi_lag(case_rate_scaled, lag = c(7, 14)) %>% # cases
    step_epi_ahead(case_rate_scaled, ahead = 7, role = "outcome") %>% # cases
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  f <- frosting() %>%
    layer_predict() %>%
    layer_threshold(.pred) %>%
    layer_naomit(.pred) %>%
    layer_population_scaling(.pred, df = reverse_pop_data,
                             by =  c("nothere" = "nope"),
                             df_pop_col = "values")


  latest <- get_test_data(recipe = r,
                          x = case_death_rate_subset %>%
                            dplyr::filter(time_value > "2021-11-01",
                                          geo_value %in% c("ca", "ny")) %>%
                            dplyr::select(geo_value, time_value, case_rate))

  wf <- epi_workflow(r,
                     parsnip::linear_reg()) %>%
    fit(jhu) %>%
    add_frosting(f)

  expect_error(p <- predict(wf, latest),
               "columns in `by` selectors of `layer_population_scaling` must be present in data and match"
                )
})

