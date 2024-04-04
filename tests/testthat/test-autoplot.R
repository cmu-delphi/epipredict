test_that("autoplot snapshots", {
  jhu <- case_death_rate_subset %>%
    filter(time_value >= as.Date("2021-11-01"))

  r <- epi_recipe(jhu) %>%
    step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
    step_epi_ahead(death_rate, ahead = 7) %>%
    step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
    step_epi_naomit()

  f <- frosting() %>%
    layer_residual_quantiles(
      quantile_levels = c(.025, .1, .25, .75, .9, .975)
    ) %>%
    layer_threshold(dplyr::starts_with(".pred")) %>%
    layer_add_target_date()

  wf <- epi_workflow(r, parsnip::linear_reg(), f) %>% fit(jhu)
  p <- autoplot(wf)
  withr::with_file("autoplot.png", {
    ggsave("autoplot.png", p)
    expect_snapshot_file("autoplot.png")
  })

  latest <- jhu %>% dplyr::filter(time_value >= max(time_value) - 14)
  preds <- predict(wf, latest)
  p <- autoplot(wf, preds, .max_facets = 4)
  withr::with_file("autoplot2.png", {
    ggsave("autoplot2.png", p)
    expect_snapshot_file("autoplot2.png")
  })
})
