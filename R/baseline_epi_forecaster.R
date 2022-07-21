baseline_epi_forecaster <- function(epi_data,
                                    outcome,
                                    args_list = baseline_args_list()) {

  ## flesh this out
  validate_forecaster_inputs(epi_data, outcome, "time_value")

  r <- epi_recipe(epi_data) %>%
    step_epi_naomit()

  fd <- max(epi_data$time_value)
  td <- fd + args_list$ahead

  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantiles(
      probs = args_list$levels,
      symmetrize = args_list$symmetrize) %>%
    layer_add_forecast_date(forecast_date = fd) %>%
    layer_add_target_date(target_date = td)
  if (args_list$nonneg) f <- layer_threshold(f, dplyr::starts_with(".pred"))

  latest <- get_test_data(r, epi_data)

  wf <- epi_workflow(r, postprocessor = f) %>%
    fit(epi_data)
  list(
    predictions = predict(wf, new_data = latest),
    epi_workflow = wf
  )
}


validate_forecaster_inputs <- function(epi_data, outcome, predictors) {
  if (! epiprocess::is_epi_df(epi_data))
    cli_stop("epi_data must be an epi_df.")
  arg_is_chr(predictors)
  arg_is_chr_scalar(outcome)
  if (! outcome %in% names(epi_data))
    cli_stop("{outcome} was not found in the training data.")
  if (! all(predictors %in% names(epi_data)))
    cli_stop("At least one predictor was not found in the training data.")
  invisible(TRUE)
}
