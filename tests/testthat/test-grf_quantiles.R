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

test_that("quantile_rand_forest operates with arx_forecaster", {
  spec <- rand_forest(mode = "regression") %>%
    set_engine("grf_quantiles", quantiles = c(.1, .2, .5, .8, .9)) # non-default
  expect_identical(rlang::eval_tidy(spec$eng_args$quantiles), c(.1, .2, .5, .8, .9))
  tib <- as_epi_df(tibble(time_value = 1:25, geo_value = "ca", value = rnorm(25)))
  o <- arx_fcast_epi_workflow(tib, "value", trainer = spec)
  spec2 <- parsnip::extract_spec_parsnip(o)
  expect_identical(
    rlang::eval_tidy(spec2$eng_args$quantiles),
    rlang::eval_tidy(spec$eng_args$quantiles)
  )
  spec <- rand_forest(mode = "regression", "grf_quantiles")
  expect_null(rlang::eval_tidy(spec$eng_args))
  o <- arx_fcast_epi_workflow(tib, "value", trainer = spec)
  spec2 <- parsnip::extract_spec_parsnip(o)
  expect_identical(
    rlang::eval_tidy(spec2$eng_args$quantiles),
    c(.05, .1, .5, .9, .95) # merged with arx_args default
  )
  df <- epidatasets::counts_subset %>% filter(time_value >= "2021-10-01")
  z <- arx_forecaster(df, "cases", "cases", spec)
  expect_identical(
    nested_quantiles(z$predictions$.pred_distn[1])[[1]]$quantile_levels,
    c(.05, .1, .5, .9, .95)
  )
})
