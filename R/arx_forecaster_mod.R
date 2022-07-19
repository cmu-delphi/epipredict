arx_epi_forecaster <- function(epi_data, response,
                               ...,
                               trainer = parsnip::linear_reg(),
                               args_list = arx_args_list()) {

  r <- epi_recipe(epi_data) %>%
    step_epi_lag(..., lag = args_list$lags) %>% # hmmm, same for all predictors
    step_epi_ahead(response, ahead = args_list$ahead) %>%
  # should use the internal function (in an open PR)
    recipes::step_naomit(recipes::all_predictors()) %>%
    recipes::step_naomit(recipes::all_outcomes(), skip = TRUE)
  # should limit the training window here (in an open PR)
  # What to do if insufficient training data? Add issue.
  # remove intercept? not sure how this is implemented in tidymodels
  f <- frosting() %>%
    layer_predict() %>%
    layer_naomit(.pred) %>%
    layer_residual_quantile(
      probs = args_list$levels,
      symmetrize = args_list$symmetrize) %>%
    layer_threshold(.pred, dplyr::starts_with("q")) #, .flag = args_list$nonneg) in open PR
  # need the target date processing here

  latest <- get_test_data(r, epi_data)

  epi_workflow(r, trainer) %>% # bug, issue 72
    add_frosting(f)

}
