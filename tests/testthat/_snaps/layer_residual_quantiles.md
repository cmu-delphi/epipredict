# Errors when used with a classifier

    Code
      forecast(wf)
    Condition
      Error in `grab_residuals()`:
      ! For meaningful residuals, the predictor should be a regression model.

# by_key names the keys without usable residuals

    Code
      flatline_forecaster(jhu_short, "death_rate", args_list = flatline_args_list(
        quantile_by_key = "geo_value"))
    Condition
      Error in `slather()`:
      ! Residual quantiles could not be calculated for some keys.
      x All training residuals are missing (`NA`) for: "tx".
      i This typically happens when a key has too few training observations for the forecast horizon.

# flatline_forecaster correctly errors when n_training < ahead

    Code
      flatline_forecaster(jhu, "death_rate", args_list = flatline_args_list(ahead = 10,
        n_training = 9))
    Condition
      Error in `slather()`:
      ! Residual quantiles could not be calculated due to missing residuals.
      i This can happen when the training window has fewer observations than the forecast horizon (for example, `n_training` < `ahead` or `n_train` < `ahead`).

