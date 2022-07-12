test_that("preprocessing steps work", {
  pop_data = data.frame(states = c("ak","al","ar","as","az","ca"),
                        value = c(1000, 2000, 3000, 4000, 5000, 6000))

  newdata = case_death_rate_subset %>% filter(geo_value %in%  c("ak","al","ar","as","az","ca"))

  r <- epi_recipe(newdata) %>%
    step_population_scaling(df = pop_data, by = c("geo_value" = "states"),
                            df_pop_col = c("value","value"),
                            x_scale_col = c("case_rate","death_rate"),
                            overwrite = FALSE) %>%
    step_epi_lag(death_rate_scaled, lag = c(0, 7, 14))

   prep(r, newdata)
  # wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(newdata)
  # latest <- newdata %>%
  #   dplyr::filter(time_value >= max(time_value) - 14)
  #
  # f <- frosting() %>% layer_predict()
  # wf1 <- wf %>% add_frosting(f)


})
