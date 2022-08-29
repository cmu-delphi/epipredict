toy_epi_df <- tibble::tibble(
  x = 1:200, y = 1:200,
  time_value = rep(seq(as.Date("2020-01-01"), by = 1,
                       length.out = 100), times = 2),
  geo_value = rep(c("ca", "hi"), each = 100)
) %>% epiprocess::as_epi_df()


test_that("step_training_window works with default n_recent", {
  p <- epi_recipe(y ~ x, data = toy_epi_df) %>%
    step_training_window() %>%
    recipes::prep(toy_epi_df) %>%
    recipes::bake(new_data = NULL)

  expect_equal(nrow(p), 100L)
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_named(p, c("x", "y", "time_value", "geo_value"))
  expect_equal(p$time_value,
               rep(seq(as.Date("2020-02-20"), as.Date("2020-04-09"), by = 1), times = 2))
  expect_equal(p$geo_value, rep(c("ca", "hi"), each = 50))
})

test_that("step_training_window works with specified n_recent", {
  p2 <- epi_recipe(y ~ x, data = toy_epi_df) %>%
    step_training_window(n_recent = 5) %>%
    recipes::prep(toy_epi_df) %>%
    recipes::bake(new_data = NULL)

  expect_equal(nrow(p2), 10L)
  expect_equal(ncol(p2), 4L)
  expect_s3_class(p2, "epi_df")
  expect_named(p2, c("x", "y", "time_value", "geo_value"))
  expect_equal(p2$time_value,
               rep(seq(as.Date("2020-04-05"), as.Date("2020-04-09"), by = 1), times = 2))
  expect_equal(p2$geo_value, rep(c("ca", "hi"), each = 5))
})

test_that("step_training_window does not proceed with specified new_data", {
# Should just return whatever the new_data is, unaffected by the step
  p3 <- epi_recipe(y ~ x, data = toy_epi_df) %>%
    step_training_window(n_recent = 3) %>%
    recipes::prep(toy_epi_df) %>%
    recipes::bake(new_data = toy_epi_df[1:10,])

  expect_equal(nrow(p3), 10L)
  expect_equal(ncol(p3), 4L)
  expect_s3_class(p3, "epi_df")
  expect_named(p3, c("x", "y", "time_value", "geo_value"))
  expect_equal(p3$time_value,
               rep(seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = 1), times = 1))
  expect_equal(p3$geo_value, rep("ca", times = 10))
})

test_that("step_training_window works with multiple keys", {
  toy_epi_df2 <- tibble::tibble(
    x = 1:200, y = 1:200,
    time_value = rep(seq(as.Date("2020-01-01"), by = 1,
                         length.out = 100), times = 2),
    geo_value = rep(c("ca", "hi"), each = 100),
    additional_key = as.factor(rep(1:4, each = 50)),
  ) %>% epiprocess::as_epi_df()

  attributes(toy_epi_df2)$metadata$other_keys <- "additional_key"

  p4 <- epi_recipe(y ~ x, data = toy_epi_df2) %>%
    step_training_window(n_recent = 3) %>%
    recipes::prep(toy_epi_df2) %>%
    recipes::bake(new_data = NULL)

  expect_equal(nrow(p4), 12L)
  expect_equal(ncol(p4), 5L)
  expect_s3_class(p4, "epi_df")
  expect_named(p4, c("x", "y", "time_value", "geo_value", "additional_key"))
  expect_equal(p4$time_value,
               rep(c(seq(as.Date("2020-02-17"), as.Date("2020-02-19"), length.out = 3),
                                   seq(as.Date("2020-04-07"), as.Date("2020-04-09"),
                                       length.out = 3)), times = 2))
  expect_equal(p4$geo_value, rep(c("ca", "hi"), each = 6))
})
