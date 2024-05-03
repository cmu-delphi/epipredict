# epipredict (development)

Pre-1.0.0 numbering scheme: 0.x will indicate releases, while 0.0.x will indicate PR's.
# epipredict 0.2

-   add `latency_adjustment` as an option for `add_epi_ahead`, which adjusts the `ahead` so that the prediction is `ahead` relative to the `as_of` date for the `epi_data`, rather than relative to the last day of data.

# epipredict 0.1

- simplify `layer_residual_quantiles()` to avoid timesuck in `utils::methods()`
- rename the `dist_quantiles()` to be more descriptive, breaking change
- removes previous `pivot_quantiles()` (now `*_wider()`, breaking change)
- add `pivot_quantiles_wider()` for easier plotting
- add complement `pivot_quantiles_longer()`
- add `cdc_baseline_forecaster()` and `flusight_hub_formatter()`
- add `smooth_quantile_reg()`
- improved printing of various methods / internals
- canned forecasters get a class
- fixed quantile bug in `flatline_forecaster()`
- add functionality to output the unfit workflow from the canned forecasters
- add quantile_reg()
- clean up documentation bugs
- add smooth_quantile_reg()
- add classifier
- training window step debugged
- `min_train_window` argument removed from canned forecasters
- add forecasters
- implement postprocessing
- vignettes avaliable
- arx_forecaster
- pkgdown
- Publish public for easy navigation
- Two simple forecasters as test beds
- Working vignette
- use `checkmate` for input validation
- refactor quantile extrapolation (possibly creates different results)
- force `target_date` + `forecast_date` handling to match the time_type of the
  epi_df. allows for annual and weekly data
- add `check_enough_train_data()` that will error if training data is too small
- added `check_enough_train_data()` to `arx_forecaster()`
- `layer_residual_quantiles()` will now error if any of the residual quantiles
  are NA
- `*_args_list()` functions now warn if `forecast_date + ahead != target_date`
- the `predictor` argument in `arx_forecaster()` now defaults to the value of
  the `outcome` argument
- `arx_fcast_epi_workflow()` and `arx_class_epi_workflow()` now default to
  `trainer = parsnip::logistic_reg()` to match their more canned versions.
- add a `forecast()` method simplify generating forecasts
- refactor `bake.epi_recipe()` and remove `epi_juice()`. 
- Revise `compat-purrr` to use the r-lang `standalone-*` version (via 
  `{usethis}`)
