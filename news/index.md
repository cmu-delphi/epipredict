# Changelog

## epipredict 0.2.7

- [`autoplot.canned_epipred()`](https://cmu-delphi.github.io/epipredict/reference/autoplot-epipred.md)
  had internal code using
  `epiprocess:::autoplot.epi_df(.max_facets = Inf)`. This argument no
  longer exists there, so it caused an error (noted by rebuilding the
  `README.Rmd`)

## epipredict 0.2.6

- [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
  and
  [`flatline_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/flatline_forecaster.md)
  now error early when `quantile_by_key` contains columns that are not
  keys of the input `epi_df`, rather than silently dropping the invalid
  keys ([\#229](https://github.com/cmu-delphi/epipredict/issues/229)).
- [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
  now warns when `quantile_by_key` is supplied with a quantile-output
  trainer
  ([`quantile_reg()`](https://cmu-delphi.github.io/epipredict/reference/quantile_reg.md),
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  with engine `"grf_quantiles"`), where the argument would otherwise be
  silently ignored
  ([\#229](https://github.com/cmu-delphi/epipredict/issues/229)).
- Regenerate roxygen-derived `man/step_adjust_latency.Rd` so its
  recorded example output matches the current `epi_df` print phrasing
  (`lag` → `latency`) from upstream `epiprocess`. No user-visible
  behavior change.
- Bump GitHub action checkout version.

## epipredict 0.2.5

- Fix
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
  and
  [`arx_fcast_epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/arx_fcast_epi_workflow.md)
  so that the error raised when `forecast_date + ahead != target_date`
  reports the actual validation message rather than a cryptic `cli`
  template-evaluation error
  ([\#473](https://github.com/cmu-delphi/epipredict/issues/473)).

## epipredict 0.2.4

- Fix
  [`flatline_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/flatline_forecaster.md)
  to return one prediction per geographic key when the input `epi_df`
  has trailing rows with `NA`s in the outcome
  ([\#454](https://github.com/cmu-delphi/epipredict/issues/454)).
  Previously, the forecast was duplicated once per trailing-NA day.

## epipredict 0.2.3

- Fix `print.canned_epipred()` so the latency-adjustment information
  actually displays for canned forecasters that include
  `step_adjust_latency` in their recipe
  ([\#447](https://github.com/cmu-delphi/epipredict/issues/447)).

## epipredict 0.2.2

- Fix
  [`autoplot.epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/autoplot-epipred.md)
  to correctly handle the response variable and avoid errors related to
  `.response`.
- Prevent subsampling in
  [`autoplot.epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/autoplot-epipred.md)
  by setting `.max_keys = Inf`.

## epipredict 0.2.1

- Fix bug in
  [`flusight_hub_formatter()`](https://cmu-delphi.github.io/epipredict/reference/flusight_hub_formatter.md)
  so that it works as expected even if the user has not first loaded the
  `epidatasets` package.

## epipredict 0.2

### Breaking changes

- Moved example datasets from being hosted in the package to being
  loaded from the `epidatasets` package. The datasets can no longer be
  loaded with `data(<dataset name>)`, but can be accessed with
  `data(<dataset name>, package = "epidatasets")`,
  `epidatasets::<dataset name>` or, after loading the package, the name
  of the dataset alone
  ([\#382](https://github.com/cmu-delphi/epipredict/issues/382)).
- [`step_adjust_latency()`](https://cmu-delphi.github.io/epipredict/reference/step_adjust_latency.md)
  no longer allows empty column selection.
- Addresses upstream breaking changes from cmu-delphi/epiprocess#595
  ([`growth_rate()`](https://cmu-delphi.github.io/epiprocess/reference/growth_rate.html)).
  [`step_growth_rate()`](https://cmu-delphi.github.io/epipredict/reference/step_growth_rate.md)
  has lost its `additional_gr_args_list` argument and now has an `na_rm`
  argument.
- Moves `epiprocess` out of depends
  ([\#440](https://github.com/cmu-delphi/epipredict/issues/440)). No
  internals have changed, but downstream users may need to add
  [`library(epiprocess)`](https://github.com/cmu-delphi/epiprocess) to
  existing code.
- Removes dependence on the `distributional` package, replacing the
  quantiles with
  [`hardhat::quantile_pred()`](https://hardhat.tidymodels.org/reference/quantile_pred.html).
  Some associated functions are deprecated with `lifecycle` messages.
- Rename `check_enough_train_data()` to
  [`check_enough_data()`](https://cmu-delphi.github.io/epipredict/reference/check_enough_data.md),
  and generalize it enough to use as a check on either training or
  testing.
- Add check for enough data to predict in
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
- Adds the `.facet_filter` option in
  [`epiprocess::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
  (cmu-delphi/epiprocess#647).

### Improvements

- Add `step_adjust_latency`, which give several methods to adjust the
  forecast if the `forecast_date` is after the last day of data.
- Fix `layer_population_scaling` default `by` with `other_keys`.
- Make key column inference more consistent within the package and with
  current `epiprocess`.
- Fix
  [`quantile_reg()`](https://cmu-delphi.github.io/epipredict/reference/quantile_reg.md)
  producing error when asked to output just median-level predictions.
- (temporary) ahead negative is allowed for `step_epi_ahead` until we
  have `step_epi_shift`
- Add `reference_date` as an argument to
  [`epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/epi_recipe.md)
- Add
  [`step_climate()`](https://cmu-delphi.github.io/epipredict/reference/step_climate.md)
  to create “climate” predictor in forecast workflows
- Add
  [`climatological_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/climatological_forecaster.md)
  to automatically create climate baselines
- Replace
  [`dist_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/dist_quantiles.md)
  with
  [`hardhat::quantile_pred()`](https://hardhat.tidymodels.org/reference/quantile_pred.html)
- Allow [`quantile()`](https://rdrr.io/r/stats/quantile.html) to
  threshold to an interval if desired
  ([\#434](https://github.com/cmu-delphi/epipredict/issues/434))
- [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
  detects if there’s enough data to predict
- Add `observed_response` to `autoplot` so that forecasts can be plotted
  against the values they’re predicting
- [`pivot_quantiles_longer()`](https://cmu-delphi.github.io/epipredict/reference/pivot_quantiles.md)
  now appropriately adds `quantile_level` to the `epi_df` other keys

### Bug fixes

- Shifting no columns results in no error for either `step_epi_ahead`
  and `step_epi_lag`
- Quantiles produced by `grf` were sometimes out of order.
- dist_quantiles can have all `NA` values without causing unrelated
  errors
- adjust default quantiles throughout so that they match.
- force
  [`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/layer_residual_quantiles.md)
  to always include `0.5`.
- Rename `recipes:::check_training_set()` to
  `recipes:::validate_training_data()`, as it changed in recipes 1.1.0.
- A new column name duplicating an existing column name results in an
  error instead of a random name.

## epipredict 0.1

- simplify
  [`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/layer_residual_quantiles.md)
  to avoid timesuck in
  [`utils::methods()`](https://rdrr.io/r/utils/methods.html)
- rename the
  [`dist_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/dist_quantiles.md)
  to be more descriptive, breaking change
- removes previous
  [`pivot_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/pivot_quantiles.md)
  (now `*_wider()`, breaking change)
- add
  [`pivot_quantiles_wider()`](https://cmu-delphi.github.io/epipredict/reference/pivot_quantiles.md)
  for easier plotting
- add complement
  [`pivot_quantiles_longer()`](https://cmu-delphi.github.io/epipredict/reference/pivot_quantiles.md)
- add
  [`cdc_baseline_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/cdc_baseline_forecaster.md)
  and
  [`flusight_hub_formatter()`](https://cmu-delphi.github.io/epipredict/reference/flusight_hub_formatter.md)
- add
  [`smooth_quantile_reg()`](https://cmu-delphi.github.io/epipredict/reference/smooth_quantile_reg.md)
- improved printing of various methods / internals
- canned forecasters get a class
- fixed quantile bug in
  [`flatline_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/flatline_forecaster.md)
- add functionality to output the unfit workflow from the canned
  forecasters
- add quantile_reg()
- clean up documentation bugs
- add smooth_quantile_reg()
- add classifier
- training window step debugged
- `min_train_window` argument removed from canned forecasters
- add forecasters
- implement post-processing
- vignettes avaliable
- arx_forecaster
- pkgdown
- Publish public for easy navigation
- Two simple forecasters as test beds
- Working vignette
- use `checkmate` for input validation
- refactor quantile extrapolation (possibly creates different results)
- force `target_date` + `forecast_date` handling to match the time_type
  of the epi_df. allows for annual and weekly data
- add `check_enough_train_data()` that will error if training data is
  too small
- added `check_enough_train_data()` to
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
- [`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/layer_residual_quantiles.md)
  will now error if any of the residual quantiles are NA
- `*_args_list()` functions now warn if
  `forecast_date + ahead != target_date`
- the `predictor` argument in
  [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
  now defaults to the value of the `outcome` argument
- [`arx_fcast_epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/arx_fcast_epi_workflow.md)
  and
  [`arx_class_epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/arx_class_epi_workflow.md)
  now default to `trainer = parsnip::logistic_reg()` to match their more
  canned versions.
- add a
  [`forecast()`](https://generics.r-lib.org/reference/forecast.html)
  method simplify generating forecasts
- refactor `bake.epi_recipe()` and remove `epi_juice()`.
- Revise `compat-purrr` to use the r-lang `standalone-*` version (via
  [usethis](https://usethis.r-lib.org))
- Replaced old version-faithful example in sliding AR & ARX forecasters
  vignette
- [`epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/epi_recipe.md)
  will now warn when given non-`epi_df` data
- [`layer_predict()`](https://cmu-delphi.github.io/epipredict/reference/layer_predict.md)
  and
  [`predict.epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/predict-epi_workflow.md)
  will now appropriately forward `...` args intended for
  [`predict.model_fit()`](https://parsnip.tidymodels.org/reference/predict.model_fit.html)
- `bake.epi_recipe()` will now re-infer the geo and time type in case
  baking the steps has changed the appropriate values
- produce length 0
  [`dist_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/dist_quantiles.md)
- add functionality to calculate weighted interval scores for
  [`dist_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/dist_quantiles.md)
- Add `step_epi_slide` to produce generic sliding computations over an
  `epi_df`
- Add quantile random forests (via
  [grf](https://github.com/grf-labs/grf)) as a parsnip engine
- Replace `epi_keys()` with
  [`epiprocess::key_colnames()`](https://cmu-delphi.github.io/epiprocess/reference/key_colnames.html),
  [\#352](https://github.com/cmu-delphi/epipredict/issues/352)
- More descriptive error messages from `arg_is_*()`,
  [\#287](https://github.com/cmu-delphi/epipredict/issues/287)
- Fix bug where [`fit()`](https://generics.r-lib.org/reference/fit.html)
  drops the `epi_workflow` class (also error if non-`epi_df` data is
  given to
  [`epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/epi_recipe.md)),
  [\#363](https://github.com/cmu-delphi/epipredict/issues/363)
- Try to retain the `epi_df` class during baking to the extent possible,
  [\#376](https://github.com/cmu-delphi/epipredict/issues/376)
