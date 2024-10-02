# Errors when used with a classifier

    Code
      forecast(wf)
    Condition
      Error in `grab_residuals()`:
      ! For meaningful residuals, the predictor should be a regression model.

# flatline_forecaster correctly errors when n_training < ahead

    Code
      flatline_forecaster(jhu, "death_rate", args_list = flatline_args_list(ahead = 10,
        n_training = 9))
    Condition
      Error in `slather()`:
      ! Residual quantiles could not be calculated due to missing residuals.
      i This may be due to `n_train` < `ahead` in your <epi_recipe>.

