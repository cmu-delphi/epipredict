library(dplyr)

tt <- seq(as.Date("2022-01-01"), by = "1 day", length.out = 20)
edf <- data.frame(
  time_value = c(tt, tt),
  geo_value = rep(c("ca", "ny"), each = 20L),
  value = c(2:21, 3:22)
) %>%
  as_epi_df()
r <- epi_recipe(edf)


test_that("epi_slide errors when needed", {
  # not an epi_recipe
  expect_snapshot(error = TRUE, recipe(edf) %>% step_epi_slide(value, .f = mean, .window_size = 7L))

  # non-scalar args
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = c(3L, 6L)))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .align = c("right", "left")))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, skip = c(TRUE, FALSE)))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, role = letters[1:2]))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, prefix = letters[1:2]))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, id = letters[1:2]))
  # wrong types
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1.5))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, .align = 1.5))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, skip = "a"))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, role = 1))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, prefix = 1))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = mean, .window_size = 1L, id = 1))
  # function problems
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value))
  expect_snapshot(error = TRUE, r %>% step_epi_slide(value, .f = 1))
})


test_that("epi_slide handles different function specs", {
  cfun <- r %>%
    step_epi_slide(value, .f = "mean", .window_size = 4L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expected_out <- edf %>%
    group_by(geo_value) %>%
    epi_slide(~ mean(.x$value), .window_size = 4L) %>%
    ungroup() %>%
    rename(epi_slide__.f_value = slide_value)
  expect_equal(cfun, expected_out)
  ffun <- r %>%
    step_epi_slide(value, .f = mean, .window_size = 4L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(ffun, expected_out)
  # formula NOT currently supported
  expect_snapshot(
    error = TRUE,
    lfun <- r %>%
      step_epi_slide(value, .f = ~ mean(.x, na.rm = TRUE), .window_size = 4L)
  )
  # expect_equal(lfun, rolled_before)
  blfun <- r %>%
    step_epi_slide(value, .f = function(x) mean(x, na.rm = TRUE), .window_size = 4L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expected_out <- edf %>%
    group_by(geo_value) %>%
    epi_slide(~ mean(.x$value, na.rm = TRUE), .window_size = 4L) %>%
    ungroup() %>%
    rename(epi_slide__.f_value = slide_value)
  expect_equal(blfun, expected_out)
  nblfun <- r %>%
    step_epi_slide(value, .f = \(x) mean(x, na.rm = TRUE), .window_size = 4L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(nblfun, expected_out)
})
