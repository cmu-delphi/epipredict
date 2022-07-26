baseline_epi_forecaster <- function(epi_data,
                                    outcome,
                                    args_list = baseline_args_list()) {

  validate_forecaster_inputs(epi_data, outcome, "time_value")
  ek <- epi_keys(epi_data)[-1]
  form <- as.formula(paste0(outcome, " ~ ", paste0(ek, collapse = ":")))

  train_data <- epi_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ek))) %>%
    dplyr::group_modify(~ {
      dplyr::filter(.x, !is.na(!!outcome)) %>%
        tail(args_list$n_recent)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c("time_value", ek, outcome))) %>%
    dplyr::mutate(time_value = max(time_value, na.rm = TRUE))

  latest <- train_data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(ek))) %>%
    dplyr::group_modify(~ tail(.x, 1L)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(c("time_value", ek)))

  r <- epi_recipe(form, train_data) %>%
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

  wf <- epi_workflow(r, parsnip::linear_reg(), f) %>% fit(train_data)

  latest <-

  list(
    predictions = suppressWarnings(predict(wf, new_data = latest)),
    epi_workflow = wf
  )
}

baseline_args_list <- function(ahead = 7L,
                               n_recent = 1L,
                               min_train_window = 20L,
                               levels = c(0.05, 0.95),
                               symmetrize = TRUE,
                               nonneg = TRUE) {

  arg_is_scalar(ahead, min_train_window, n_recent)
  arg_is_nonneg_int(ahead, min_train_window, n_recent)
  arg_is_lgl(symmetrize, nonneg)
  arg_is_probabilities(levels, allow_null = TRUE)
  if (n_recent > min_train_window)
    rlang::warn("min_train_window is smaller than n_recent")

  list(ahead = ahead, n_recent = n_recent,
       min_train_window = min_train_window, levels = levels,
       symmetrize = symmetrize, nonneg = nonneg)
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

