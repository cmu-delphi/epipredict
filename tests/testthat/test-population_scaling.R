library(recipes)
library(parsnip)
library(workflows)

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
  expect_equal(ncol(b), 8L)
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
  expect_equal(ncol(b), 6L)

})



