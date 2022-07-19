library(recipes)
library(parsnip)
library(workflows)

## Preprocessing
test_that("Column names can be passed with and without the tidy way", {
  pop_data = data.frame(states = c("ak","al","ar","as","az","ca"),
                        value = c(1000, 2000, 3000, 4000, 5000, 6000))

  newdata = case_death_rate_subset %>% filter(geo_value %in%  c("ak","al","ar","as","az","ca"))

  r1 <- epi_recipe(newdata) %>%
    step_population_scaling(df = pop_data,
                            df_pop_col = "value", by = c("geo_value" = "states"),
                             c("case_rate", "death_rate"))

  r2 <- epi_recipe(newdata) %>%
    step_population_scaling(df = pop_data,
                            df_pop_col = "value", by = c("geo_value" = "states"),
                            case_rate, death_rate)

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
    step_population_scaling(df = pop_data,
                            df_pop_col = "value", by = c("geo_value" = "states", "county" = "counties"),
                            c("case", "death"), suffix = "_rate")

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
test_that("Postprocessing works", {
  jhu <- epiprocess::jhu_csse_daily_subset %>%
    dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ca", "ny")) %>%
    dplyr::select(geo_value, time_value, cases)

  pop_data = data.frame(states = c("ca", "ny"),
                        value = c(20000, 30000))

  r <- epi_recipe(jhu) %>%
    step_population_scaling(df = pop_data,
                            df_pop_col = "value",
                            by = c("geo_value" = "states"),
                            cases, suffix = "_scaled") %>%
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
#  expect_equal(nrow(p), 2L)
#  expect_equal(ncol(p), 3L)


})


