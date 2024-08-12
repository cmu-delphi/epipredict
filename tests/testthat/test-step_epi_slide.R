library(dplyr)

tt <- seq(as.Date("2022-01-01"), by = "1 day", length.out = 20)
edf <- data.frame(
  time_value = c(tt, tt),
  geo_value = rep(c("ca", "ny"), each = 20L),
  value = c(2:21, 3:22)
) %>%
  as_epi_df()

r <- epi_recipe(edf)
rolled_before <- edf %>%
  group_by(geo_value) %>%
  epi_slide(value = mean(value), before = 3L) %>%
  pull(value)
rolled_after <- edf %>%
  group_by(geo_value) %>%
  epi_slide(value = mean(value), after = 3L) %>%
  pull(value)


test_that("epi_slide errors when needed", {
  # not an epi_recipe
  expect_error(recipe(edf) %>% step_epi_slide(value, .f = mean, before = 6L))

  # non-scalar args
  expect_error(r %>% step_epi_slide(value, .f = mean, before = c(3L, 6L)))
  expect_error(r %>% step_epi_slide(value, .f = mean, after = c(3L, 6L)))
  expect_error(r %>% step_epi_slide(value, .f = mean, skip = c(TRUE, FALSE)))
  expect_error(r %>% step_epi_slide(value, .f = mean, role = letters[1:2]))
  expect_error(r %>% step_epi_slide(value, .f = mean, prefix = letters[1:2]))
  expect_error(r %>% step_epi_slide(value, .f = mean, id = letters[1:2]))
  # wrong types
  expect_error(r %>% step_epi_slide(value, .f = mean, before = 1.5))
  expect_error(r %>% step_epi_slide(value, .f = mean, after = 1.5))
  expect_error(r %>% step_epi_slide(value, .f = mean, skip = "a"))
  expect_error(r %>% step_epi_slide(value, .f = mean, role = 1))
  expect_error(r %>% step_epi_slide(value, .f = mean, prefix = 1))
  expect_error(r %>% step_epi_slide(value, .f = mean, id = 1))
  # function problems
  expect_error(r %>% step_epi_slide(value))
  expect_error(r %>% step_epi_slide(value, .f = 1))
})


test_that("epi_slide handles different function specs", {
  cfun <- r %>%
    step_epi_slide(value, .f = "mean", before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  ffun <- r %>%
    step_epi_slide(value, .f = mean, before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  # formula NOT currently supported
  expect_error(
    lfun <- r %>%
      step_epi_slide(value, .f = ~ mean(.x, na.rm = TRUE), before = 3L),
    regexp = "cannot be a formula."
  )
  blfun <- r %>%
    step_epi_slide(value, .f = function(x) mean(x, na.rm = TRUE), before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  nblfun <- r %>%
    step_epi_slide(value, .f = \(x) mean(x, na.rm = TRUE), before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)

  expect_equal(cfun[[4]], rolled_before)
  expect_equal(ffun[[4]], rolled_before)
  # expect_equal(lfun[[4]], rolled_before)
  expect_equal(blfun[[4]], rolled_before)
  expect_equal(nblfun[[4]], rolled_before)
})
