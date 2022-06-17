tib <- tibble::tibble(
  x = 1:200, y = 1:200,
  time_value = rep(seq(as.Date("2020-01-01"), by = 1,
                       length.out = 100), times = 2),
  geo_value = rep(c("ca", "hi"), each = 100)
) %>% epiprocess::as_epi_df()

test_that("step_training_window works with default nrec", {
  p <- epi_recipe(y ~ x, data = tib) %>%
    step_training_window() %>%
    recipes::prep(tib) %>%
    recipes::bake(new_data = NULL)

  expect_equal(nrow(p), 100L)
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_named(p, c("x", "y", "time_value", "geo_value"))
  expect_equal(p$time_value, rep(seq(as.Date("2020-02-20"), as.Date("2020-04-09"), by = 1), times = 2))
  expect_equal(p$geo_value, rep(c("ca", "hi"), each = 50))
})

test_that("step_training_window works with specified nrec", {
  p2 <- epi_recipe(y ~ x, data = tib) %>%
    step_training_window(nrec = 5) %>%
    recipes::prep(tib) %>%
    recipes::bake(new_data = NULL)

  expect_equal(nrow(p2), 10L)
  expect_equal(ncol(p2), 4L)
  expect_s3_class(p2, "epi_df")
  expect_named(p2, c("x", "y", "time_value", "geo_value"))
  expect_equal(p2$time_value, rep(seq(as.Date("2020-04-05"), as.Date("2020-04-09"), by = 1), times = 2))
  expect_equal(p2$geo_value, rep(c("ca", "hi"), each = 5))
})

test_that("step_training_window does not proceed with specified new_data", {
# Should just return whatever the new_data is, unaffected by the step
  p3 <- epi_recipe(y ~ x, data = tib) %>%
    step_training_window(nrec = 3) %>%
    recipes::prep(tib) %>%
    recipes::bake(new_data = tib[1:10,])

  expect_equal(nrow(p3), 10L)
  expect_equal(ncol(p3), 4L)
  expect_s3_class(p3, "epi_df")
  expect_named(p3, c("x", "y", "time_value", "geo_value"))
  expect_equal(p3$time_value, rep(seq(as.Date("2020-01-01"), as.Date("2020-01-10"), by = 1), times = 1))
  expect_equal(p3$geo_value, rep("ca", times = 10))
})
