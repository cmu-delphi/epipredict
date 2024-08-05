tt <- seq(as.Date("2022-01-01"), by = "1 day", length.out = 20)
edf <- data.frame(
  time_value = c(tt, tt),
  geo_value = rep(c("ca", "ny"), each = 20L),
  value = c(2:21, 3:22),
  value2 = c(5+(2:21),-1 + (3:22))
) %>%
  as_epi_df()

r <- epi_recipe(edf)

library(dplyr)
rolled_before_epi_slide <- edf %>%
  group_by(geo_value) %>%
  epi_slide(epi_slide_mean_value = mean(value), before = 3L)
rolled_before <- edf %>%
  group_by(geo_value) %>%
  epi_slide_mean("value", before = 3L) %>%
  rename(epi_slide_mean_value = "slide_value_value") %>%
  ungroup()
rolled_after <- edf %>%
  group_by(geo_value) %>%
  epi_slide_mean("value", after = 3L) %>%
  rename(epi_slide_mean_value = "slide_value_value") %>%
  ungroup()

test_that("epi_slide handles classed before/after", {
  baseline <- r %>%
    step_epi_slide_mean(value, before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline, rolled_before)

  pbefore <- r %>%
    step_epi_slide_mean(value, before = lubridate::period("3 days")) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  cbefore <- r %>%
    step_epi_slide_mean(value, before = "3 days") %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline, pbefore)
  expect_equal(baseline, cbefore)

  baseline <- r %>%
    step_epi_slide_mean(value, after = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline, rolled_after)
  pafter <- r %>%
    step_epi_slide_mean(value, after = lubridate::period("3 days")) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  cafter <- r %>%
    step_epi_slide_mean(value, after = "3 days") %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline, pafter)
  expect_equal(baseline, cafter)
})

test_that("epi_slide_mean has different edge behavior than epi_slide", {
  # the reason base epi_slide and epi_slide_sum aren't identical is because
  # epi_slide doesn't return `NA`'s when the window is short
  # so we expect the comparison across all entries to return `NA`
  res_mean_specific <- r %>%
    step_epi_slide_mean(value, before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_true(is.na(all(rolled_before_epi_slide == res_mean_specific)))
})

rolled_before_2 <- edf %>%
  group_by(geo_value) %>%
  epi_slide_mean(c("value", "value2"), before = 3L) %>%
  ungroup() %>%
  rename(
    c(epi_slide_mean_value="slide_value_value",
      epi_slide_mean_value2="slide_value_value2")
  )

test_that("handles multiple columns correctly", {
  stepped <- r %>%
    step_epi_slide_mean(starts_with("value"), before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(stepped, rolled_before_2)
})
