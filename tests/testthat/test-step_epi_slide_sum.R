tt <- seq(as.Date("2022-01-01"), by = "1 day", length.out = 20)
edf <- data.frame(
  time_value = c(tt, tt),
  geo_value = rep(c("ca", "ny"), each = 20L),
  value = c(2:21, 3:22),
  value2 = c(5+(2:21),-1 + (3:22))
) %>%
  as_epi_df(as_of=Sys.Date())

r <- epi_recipe(edf)

library(dplyr)
rolled_before_epi_slide <- edf %>%
  group_by(geo_value) %>%
  epi_slide(epi_slide_sum_value = sum(value), before = 3L)
rolled_before <- edf %>%
  group_by(geo_value) %>%
  epi_slide_sum("value", before = 3L) %>%
  ungroup() %>%
  rename(epi_slide_sum_value = "slide_value_value")
rolled_after <- edf %>%
  group_by(geo_value) %>%
  epi_slide_sum("value", after = 3L) %>%
  pull(slide_value_value)

# many of the properties here are actually tested in `epi_slide_mean`, so the tests aren't repeated here
test_that("epi_slide_sum works correctly", {
  baseline <- r %>%
    step_epi_slide_sum(value, before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  expect_equal(baseline, rolled_before)
  # the reason base epi_slide and epi_slide_sum aren't identical is because
  # epi_slide doesn't return `NA`'s when the window is short
  # so we expect the comparison across all entries to return `NA`
  expect_true(is.na(all(rolled_before_epi_slide == baseline)))
})

rolled_before_2 <- edf %>%
  group_by(geo_value) %>%
  epi_slide_sum(c("value", "value2"), before = 3L) %>%
  ungroup() %>%
  rename(
    c(epi_slide_sum_value="slide_value_value",
      epi_slide_sum_value2="slide_value_value2")
  )

test_that("handles multiple columns correctly", {
  stepped <- r %>%
    step_epi_slide_sum(starts_with("value"), before = 3L) %>%
    prep(edf) %>%
    bake(new_data = NULL)
  stepped
  expect_equal(stepped, rolled_before_2)

})
