tt <- seq(as.Date("2022-01-01"), by = "1 day", length.out = 20)
edf <- data.frame(
  time_value = c(tt, tt),
  geo_value = rep(c("ca", "ny"), each = 20L),
  value = c(2:21, 3:22)
) %>%
  as_epi_df()

r <- epi_recipe(edf)

test_that("try_period works", {
  expect_error(try_period("1 jeff"))
  expect_error(try_period(lubridate::period("1 jeff")))
  expect_error(try_period(NA))
  expect_error(try_period(1.5))
  res <- lubridate::weeks(1)
  expect_identical(try_period("1 week"), res)
  expect_identical(try_period(lubridate::period("1 week")), res)
  expect_identical(try_period(1L), 1L)
})


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

library(dplyr)
rolled_before <- edf %>%
  group_by(geo_value) %>%
  epi_slide(value = mean(value), before = 3L) %>%
  pull(value)
rolled_after <- edf %>%
  group_by(geo_value) %>%
  epi_slide(value = mean(value), after = 3L) %>%
  pull(value)


test_that("epi_slide handles classed before/after", {

  baseline <- r %>%
    step_epi_slide(value, .f = mean, before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline[[4]], rolled_before)

  pbefore <- r %>%
    step_epi_slide(value, .f = mean, before = lubridate::period("3 days")) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  cbefore <- r %>%
    step_epi_slide(value, .f = mean, before = "3 days") %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline, pbefore)
  expect_equal(baseline, cbefore)

  baseline <- r %>%
    step_epi_slide(value, .f = mean, after = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline[[4]], rolled_after)
  pafter <- r %>%
    step_epi_slide(value, .f = mean, after = lubridate::period("3 days")) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  cafter <- r %>%
    step_epi_slide(value, .f = mean, after = "3 days") %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline, pafter)
  expect_equal(baseline, cafter)
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
  lfun <- r %>%
    step_epi_slide(value, .f = ~ mean(.x, na.rm = TRUE), before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
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
  expect_equal(lfun[[4]], rolled_before)
  expect_equal(blfun[[4]], rolled_before)
  expect_equal(nblfun[[4]], rolled_before)
})
