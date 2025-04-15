library(tidyr)
tib <- expand_grid(
  tv = 1:4,
  gv = letters[1:2],
  cl = letters[2:4]
)
tib$val1 <- 1:nrow(tib) + .1
tib$val2 <- nrow(tib):1 - .1


test_that("works with recipe, various possible pivots", {
  out <- recipe(tib) %>%
    step_pivot_wider(val1, names_from = cl) %>%
    prep(training = tib) %>% bake(new_data = NULL)
  expect_snapshot(out)

  out <- recipe(tib) %>%
    step_pivot_wider(val1, names_from = cl, values_fill = 0) %>%
    prep(training = tib) %>% bake(new_data = NULL)
  expect_snapshot(out)

  out <- recipe(tib) %>%
    step_pivot_wider(starts_with("val"), names_from = cl) %>%
    prep(training = tib) %>% bake(new_data = NULL)
  expect_snapshot(out)

  out <- recipe(tib) %>%
    step_pivot_wider(val1, names_from = gv:cl) %>%
    prep(training = tib) %>% bake(new_data = NULL)
  expect_snapshot(out)

  #fails
  out <- recipe(tib) %>%
    step_pivot_wider(val1, id_cols = tv:cl, names_from = cl) %>%
    prep(training = tib) %>% bake(new_data = NULL)

  edf <- tib %>% as_epi_df()
})
