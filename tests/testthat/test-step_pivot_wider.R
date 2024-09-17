library(tidyr)
tib <- expand_grid(
  tv = 1:4,
  gv = letters[1:2],
  cl = letters[2:4]
)
tib$val1 <- rnorm(nrow(tib))
tib$val2 <- rnorm(nrow(tib))

recipe(tib) %>%
  step_pivot_wider(starts_with(val),
    names_from = cl,
    values_fill = list(val1 = 0, val2 = 0)
  ) %>%
  prep()
  bake(new_data = NULL)

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
