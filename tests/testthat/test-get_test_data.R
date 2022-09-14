library(dplyr)
test_that("return expected number of rows and returned dataset is ungrouped", {
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14, 21, 28)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  test <- get_test_data(recipe = r, x = case_death_rate_subset)

  expect_equal(nrow(test),
               dplyr::n_distinct(case_death_rate_subset$geo_value) * 29)

  expect_false(dplyr::is.grouped_df(test))
})


test_that("expect insufficient training data error", {
  r <- epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 367)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  expect_error(get_test_data(recipe = r, x = case_death_rate_subset))
})

test_that("expect error that geo_value or time_value does not exist", {
  r <-  epi_recipe(case_death_rate_subset) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_naomit(all_predictors()) %>%
    step_naomit(all_outcomes(), skip = TRUE)

  wrong_epi_df <- case_death_rate_subset %>% dplyr::select(-geo_value)

  expect_error(get_test_data(recipe = r, x = wrong_epi_df))
})


test_that("NA fill behaves as desired", {
 df <- tibble::tibble(
   geo_value = rep(c("ca", "ny"), each = 10),
   time_value = rep(1:10, times = 2),
   x1 = rnorm(20),
   x2 = rnorm(20)) %>%
   epiprocess::as_epi_df()

 r <- epi_recipe(df) %>%
   step_epi_ahead(x1, ahead = 3) %>%
   step_epi_lag(x1, x2, lag = c(1,3)) %>%
   step_epi_naomit()

 expect_silent(tt <- get_test_data(r, df))
 expect_s3_class(tt, "epi_df")

 expect_error(get_test_data(r, df, "A"))
 expect_error(get_test_data(r, df, TRUE, -3))

 df2 <- df
 df2$x1[df2$geo_value == "ca"] <- NA

 td <- get_test_data(r, df2)
 expect_true(any(is.na(td)))
 expect_error(get_test_data(r, df2, TRUE))

 df1 <- df2
 df1$x1[1:4] <- 1:4
 td1 <- get_test_data(r, df, TRUE, n_recent = 6)
 expect_true(!any(is.na(td1)))

 df2$x1[7:8] <- 1:2
 td2 <- get_test_data(r, df2, TRUE)
 expect_true(!any(is.na(td2)))


})
