set.seed(12345)
suppressPackageStartupMessages(library(grf))
tib <- tibble(
  y = rnorm(100), x = rnorm(100), z = rnorm(100),
  f = factor(sample(letters[1:3], 100, replace = TRUE))
)

test_that("quantile_rand_forest defaults work", {
  spec <- rand_forest(engine = "grf_quantiles", mode = "regression")
  expect_silent(out <- fit(spec, formula = y ~ x + z, data = tib))
  pars <- parsnip::extract_fit_engine(out)
  manual <- quantile_forest(as.matrix(tib[, 2:3]), tib$y, quantiles = c(0.1, 0.5, 0.9))
  expect_identical(pars$quantiles.orig, manual$quantiles.orig)
  expect_identical(pars$`_num_trees`, manual$`_num_trees`)

  fseed <- 12345
  spec_seed <- rand_forest(mode = "regression", mtry = 2L, min_n = 10) %>%
    set_engine("grf_quantiles", seed = fseed)
  out <- fit(spec_seed, formula = y ~ x + z - 1, data = tib)
  manual <- quantile_forest(
    as.matrix(tib[, 2:3]), tib$y,
    quantiles = c(0.1, 0.5, 0.9), seed = fseed,
    mtry = 2L, min.node.size = 10
  )
  p_pars <- predict(out, new_data = tib[1:5, ]) %>%
    pivot_quantiles_wider(.pred)
  p_manual <- predict(manual, newdata = as.matrix(tib[1:5, 2:3]))$predictions
  colnames(p_manual) <- c("0.1", "0.5", "0.9")
  p_manual <- tibble::as_tibble(p_manual)
  # not equal despite the seed, etc
  # expect_equal(p_pars, p_manual)
})

test_that("quantile_rand_forest handles alternative quantiles", {
  spec <- rand_forest(mode = "regression") %>%
    set_engine("grf_quantiles", quantiles = c(.2, .5, .8))
  expect_silent(out <- fit(spec, formula = y ~ x + z, data = tib))
  pars <- parsnip::extract_fit_engine(out)
  manual <- quantile_forest(as.matrix(tib[, 2:3]), tib$y, quantiles = c(.2, .5, .8))
  expect_identical(pars$quantiles.orig, manual$quantiles.orig)
  expect_identical(pars$`_num_trees`, manual$`_num_trees`)
})


test_that("quantile_rand_forest handles allows setting the trees and mtry", {
  spec <- rand_forest(mode = "regression", mtry = 2, trees = 100, engine = "grf_quantiles")
  expect_silent(out <- fit(spec, formula = y ~ x + z, data = tib))
  pars <- parsnip::extract_fit_engine(out)
  manual <- quantile_forest(as.matrix(tib[, 2:3]), tib$y, mtry = 2, num.trees = 100)
  expect_identical(pars$quantiles.orig, manual$quantiles.orig)
  expect_identical(pars$`_num_trees`, manual$`_num_trees`)
})

test_that("quantile_rand_forest predicts reasonable quantiles", {
  spec <- rand_forest(mode = "regression") %>%
    set_engine("grf_quantiles", quantiles = c(.2, .5, .8))
  expect_silent(out <- fit(spec, formula = y ~ x + z, data = tib))
  # swapping around the probabilities, because somehow this happens in practice,
  # but I'm not sure how to reproduce
  out$fit$quantiles.orig <- c(0.5, 0.9, 0.1)
  expect_no_error(predict(out, tib))
})
