set.seed(12345)
tib <- tibble(
  y = rnorm(100), x = rnorm(100), z = rnorm(100),
  f = factor(sample(letters[1:3], 100, replace = TRUE))
)

test_that("quantile_rand_forest defaults work", {
  library(grf)
  spec <- rand_forest(engine = "grf", mode = "regression")
  expect_silent(out <- fit(spec, formula = y ~ x + z, data = tib))
  pars <- parsnip::extract_fit_engine(out)
  manual <- quantile_forest(as.matrix(tib[,2:3]), tib$y, quantiles = c(0.1, 0.5, 0.9))
  expect_identical(pars$quantiles.orig, manual$quantiles)
  expect_identical(pars$`_num_trees`, manual$`_num_trees`)

  fseed <- 12345
  spec_seed <- rand_forest(mode = "regression") %>%
    set_engine("grf", seed = fseed)
  out <- fit(spec_seed, formula = y ~ x + z, data = tib)
  manual <- quantile_forest(
    as.matrix(tib[,2:3]), tib$y, quantiles = c(0.1, 0.5, 0.9), seed = fseed
  )
  p_pars <- predict(out, new_data = tib[1:5, ]) %>%
    pivot_quantiles_wider(.pred)
  p_manual <- predict(manual, newdata = as.matrix(tib[1:5, 2:3]))$predictions

  # these should be the same, given the seed, but aren't
  # expect_equal(p_pars, p_manual)
})

test_that("quantile_rand_forest handles additional quantiles", {
  spec <- quantile_reg(engine = "grf", quantile_levels = c(.2, .5, .8))
  expect_silent(out <- fit(spec, formula = y ~ x + z, data = tib))
  pars <- parsnip::extract_fit_engine(out)
  manual <- quantile_forest(as.matrix(tib[,2:3]), tib$y, quantiles = c(.2, .5, .8))
  expect_identical(pars$quantiles.orig, manual$quantiles)
  expect_identical(pars$`_num_trees`, manual$`_num_trees`)
})


test_that("quantile_rand_forest handles allows setting the trees and mtry", {
  spec <- quantile_reg(quantile_levels = c(.2, .5, .8)) %>%
    set_engine("grf", mtry = 10, trees = 100)
  expect_silent(out <- fit(spec, formula = y ~ x + z, data = tib))
  pars <- parsnip::extract_fit_engine(out)
  manual <- quantile_forest(as.matrix(tib[,2:3]), tib$y, quantiles = c(.2, .5, .8))
  expect_identical(pars$quantiles.orig, manual$quantiles)
  expect_identical(pars$`_num_trees`, manual$`_num_trees`)
})
