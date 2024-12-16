# epipredict (development)

Pre-1.0.0 numbering scheme: 0.x will indicate releases, while 0.0.x will indicate PR's.

# epipredict 0.2

## Breaking changes

- Moved example datasets from being hosted in the package to being loaded
  from the `epidatasets` package. The datasets can no longer be loaded with
  `data(<dataset name>)`, but can be accessed with 
  `data(<dataset name>, package = "epidatasets")`, `epidatasets::<dataset name>` 
  or, after loading the package, the name of the dataset alone (#382).

## Improvements

- Add `step_adjust_latency`, which give several methods to adjust the forecast if the `forecast_date` is after the last day of data.
- Fix `layer_population_scaling` default `by` with `other_keys`.
- Make key column inference more consistent within the package and with current `epiprocess`.
- Fix `quantile_reg()` producing error when asked to output just median-level predictions.
- (temporary) ahead negative is allowed for `step_epi_ahead` until we have `step_epi_shift`

## Bug fixes
- Shifting no columns results in no error for either `step_epi_ahead` and `step_epi_lag`
- Quantiles produced by `grf` were sometimes out of order.
- dist_quantiles can have all `NA` values without causing unrelated errors

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
- Replaced old version-faithful example in sliding AR & ARX forecasters vignette
- `epi_recipe()` will now warn when given non-`epi_df` data
- `layer_predict()` and `predict.epi_workflow()` will now appropriately forward
  `...` args intended for `predict.model_fit()`
- `bake.epi_recipe()` will now re-infer the geo and time type in case baking the
  steps has changed the appropriate values
- produce length 0 `dist_quantiles()`
- add functionality to calculate weighted interval scores for `dist_quantiles()`
- Add `step_epi_slide` to produce generic sliding computations over an `epi_df`
- Add quantile random forests (via `{grf}`) as a parsnip engine
- Replace `epi_keys()` with `epiprocess::key_colnames()`, #352
- More descriptive error messages from `arg_is_*()`, #287
- Fix bug where `fit()` drops the `epi_workflow` class (also error if
  non-`epi_df` data is given to `epi_recipe()`), #363
- Try to retain the `epi_df` class during baking to the extent possible, #376
