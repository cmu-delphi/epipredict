jhu <- case_death_rate_subset %>%
  dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
r <- epi_recipe(jhu) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_naomit(all_predictors()) %>%
  step_naomit(all_outcomes(), skip = TRUE)
wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
latest <- jhu %>%
  dplyr::filter(time_value >= max(time_value) - 14)


test_that("predict layer works alone", {
  f <- frosting() %>% layer_predict()
  wf1 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf1, latest))
  expect_equal(ncol(p), 3L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 108L)
  expect_named(p, c("geo_value", "time_value", ".pred"))
})

test_that("prediction with interval works", {
  f <- frosting() %>% layer_predict(type = "pred_int")
  wf2 <- wf %>% add_frosting(f)

  expect_silent(p <- predict(wf2, latest))
  expect_equal(ncol(p), 4L)
  expect_s3_class(p, "epi_df")
  expect_equal(nrow(p), 108L)
  expect_named(p, c("geo_value", "time_value", ".pred_lower", ".pred_upper"))
})

test_that("layer_predict dots validation", {
  # We balk at unnamed arguments, though perhaps not with the most helpful error messages:
  expect_error(
    frosting() %>% layer_predict("pred_int", list(), tibble::tibble(x = 5)),
    class = "epipredict__layer_predict__unnamed_dot"
  )
  expect_error(
    frosting() %>% layer_predict("pred_int", list(), "maybe_meant_to_be_id"),
    class = "epipredict__layer_predict__unnamed_dot"
  )
  # We allow arguments that might actually work at prediction time:
  expect_no_error(frosting() %>% layer_predict(type = "quantile", interval = "confidence"))

  # We don't detect completely-bogus arg names until predict time:
  expect_no_error(f_bad_arg <- frosting() %>% layer_predict(bogus_argument = "something"))
  wf_bad_arg <- wf %>% add_frosting(f_bad_arg)
  expect_error(predict(wf_bad_arg, latest))
  # Some argument names only apply for some prediction `type`s; we don't check for ignored arguments, and neither does workflows:
  expect_no_error(frosting() %>% layer_predict(eval_time = "preferably this would error"))

  # ^ (currently with a truly awful error message, due to an extra comma in parsnip::check_pred_type_dots)
  #
  # Unfortunately, we outright ignore attempts to pass args via `predict.epi_workflow`:
  f_predict <- frosting() %>% layer_predict()
  wf_predict <- wf %>% add_frosting(f_predict)
  expect_no_error(predict(wf_predict, latest, type = "pred_int"))
})

test_that("layer_predict dots are forwarded", {
  f_lm_int_level <- frosting() %>%
    layer_predict(type = "pred_int", level = 0.8)
  wf_lm_int_level <- wf %>% add_frosting(f_lm_int_level)
  p <- predict(wf, latest)
  p_lm_int_level <- predict(wf_lm_int_level, latest)
  expect_contains(names(p_lm_int_level), c(".pred_lower", ".pred_upper"))
  expect_equal(nrow(na.omit(p)), nrow(na.omit(p_lm_int_level)))
  expect_true(cbind(p, p_lm_int_level[c(".pred_lower", ".pred_upper")]) %>%
    na.omit() %>%
    mutate(sandwiched = .pred_lower <= .pred & .pred <= .pred_upper) %>%
    `[[`("sandwiched") %>%
    all())
  # There are many possible other valid configurations that aren't tested here.
})
