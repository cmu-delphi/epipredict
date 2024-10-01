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
  # ^ (currently with a awful error message, due to an extra comma in parsnip::check_pred_type_dots)

  # Some argument names only apply for some prediction `type`s; we don't check
  # for invalid pairings, nor does {parsnip}, so we end up producing a forecast
  # that silently ignores some arguments some of the time. ({workflows} doesn't
  # check for these either.)
  expect_no_error(frosting() %>% layer_predict(eval_time = "preferably this would error"))
})

test_that("layer_predict dots are forwarded", {
  f_lm_int_level_95 <- frosting() %>%
    layer_predict(type = "pred_int")
  f_lm_int_level_80 <- frosting() %>%
    layer_predict(type = "pred_int", level = 0.8)
  wf_lm_int_level_95 <- wf %>% add_frosting(f_lm_int_level_95)
  wf_lm_int_level_80 <- wf %>% add_frosting(f_lm_int_level_80)
  p <- predict(wf, latest)
  p_lm_int_level_95 <- predict(wf_lm_int_level_95, latest)
  p_lm_int_level_80 <- predict(wf_lm_int_level_80, latest)
  expect_contains(names(p_lm_int_level_95), c(".pred_lower", ".pred_upper"))
  expect_contains(names(p_lm_int_level_80), c(".pred_lower", ".pred_upper"))
  expect_equal(nrow(na.omit(p)), nrow(na.omit(p_lm_int_level_95)))
  expect_equal(nrow(na.omit(p)), nrow(na.omit(p_lm_int_level_80)))
  expect_true(
    cbind(
      p,
      p_lm_int_level_95 %>% dplyr::select(.pred_lower_95 = .pred_lower, .pred_upper_95 = .pred_upper),
      p_lm_int_level_80 %>% dplyr::select(.pred_lower_80 = .pred_lower, .pred_upper_80 = .pred_upper)
    ) %>%
      na.omit() %>%
      mutate(
        sandwiched =
          .pred_lower_95 <= .pred_lower_80 &
            .pred_lower_80 <= .pred &
            .pred <= .pred_upper_80 &
            .pred_upper_80 <= .pred_upper_95
      ) %>%
      `[[`("sandwiched") %>%
      all()
  )
  # There are many possible other valid configurations that aren't tested here.
})
