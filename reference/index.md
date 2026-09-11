# Package index

## Simple forecasters

Complete forecasters that produce reasonable baselines

- [`arx_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/arx_forecaster.md)
  : Direct autoregressive forecaster with covariates
- [`cdc_baseline_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/cdc_baseline_forecaster.md)
  : Predict the future with the most recent value
- [`climatological_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/climatological_forecaster.md)
  : Climatological forecaster
- [`flatline_forecaster()`](https://cmu-delphi.github.io/epipredict/reference/flatline_forecaster.md)
  : Predict the future with today's value
- [`arx_classifier()`](https://cmu-delphi.github.io/epipredict/reference/arx_classifier.md)
  : Direct autoregressive classifier with covariates

### Forecaster modifications

Constructors to modify forecaster arguments and utilities to produce
`epi_workflow` objects

- [`arx_args_list()`](https://cmu-delphi.github.io/epipredict/reference/arx_args_list.md)
  : ARX forecaster argument constructor

- [`arx_class_args_list()`](https://cmu-delphi.github.io/epipredict/reference/arx_class_args_list.md)
  : ARX classifier argument constructor

- [`cdc_baseline_args_list()`](https://cmu-delphi.github.io/epipredict/reference/cdc_baseline_args_list.md)
  : CDC baseline forecaster argument constructor

- [`climate_args_list()`](https://cmu-delphi.github.io/epipredict/reference/climate_args_list.md)
  : Climatological forecaster argument constructor

- [`flatline_args_list()`](https://cmu-delphi.github.io/epipredict/reference/flatline_args_list.md)
  : Flatline forecaster argument constructor

- [`arx_class_epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/arx_class_epi_workflow.md)
  :

  Create a template `arx_classifier` workflow

- [`arx_fcast_epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/arx_fcast_epi_workflow.md)
  :

  Create a template `arx_forecaster` workflow

## Steps and Layers

### Epi recipe preprocessing steps

Note that any [recipes](https://github.com/tidymodels/recipes)
[`step`](https://recipes.tidymodels.org/reference/index.html) is also
valid

- [`step_adjust_latency()`](https://cmu-delphi.github.io/epipredict/reference/step_adjust_latency.md)
  : Adapt the model to latent data
- [`step_climate()`](https://cmu-delphi.github.io/epipredict/reference/step_climate.md)
  : Calculate a climatological variable based on the history
- [`step_epi_naomit()`](https://cmu-delphi.github.io/epipredict/reference/step_epi_naomit.md)
  : Unified NA omission wrapper function for recipes
- [`step_epi_lag()`](https://cmu-delphi.github.io/epipredict/reference/step_epi_shift.md)
  [`step_epi_ahead()`](https://cmu-delphi.github.io/epipredict/reference/step_epi_shift.md)
  : Create a shifted predictor
- [`step_epi_slide()`](https://cmu-delphi.github.io/epipredict/reference/step_epi_slide.md)
  : Calculate a rolling window transformation
- [`step_growth_rate()`](https://cmu-delphi.github.io/epipredict/reference/step_growth_rate.md)
  : Calculate a growth rate
- [`step_lag_difference()`](https://cmu-delphi.github.io/epipredict/reference/step_lag_difference.md)
  : Calculate a lagged difference
- [`step_population_scaling()`](https://cmu-delphi.github.io/epipredict/reference/step_population_scaling.md)
  : Convert raw scale predictions to per-capita
- [`step_training_window()`](https://cmu-delphi.github.io/epipredict/reference/step_training_window.md)
  : Limits the size of the training window to the most recent
  observations

### Frosting post-processing layers

- [`layer_add_forecast_date()`](https://cmu-delphi.github.io/epipredict/reference/layer_add_forecast_date.md)
  : Post-processing step to add the forecast date

- [`layer_add_target_date()`](https://cmu-delphi.github.io/epipredict/reference/layer_add_target_date.md)
  : Post-processing step to add the target date

- [`layer_cdc_flatline_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/layer_cdc_flatline_quantiles.md)
  : CDC Flatline Forecast Quantiles

- [`layer_naomit()`](https://cmu-delphi.github.io/epipredict/reference/layer_naomit.md)
  :

  Omit `NA`s from predictions or other columns

- [`layer_point_from_distn()`](https://cmu-delphi.github.io/epipredict/reference/layer_point_from_distn.md)
  : Converts distributional forecasts to point forecasts

- [`layer_population_scaling()`](https://cmu-delphi.github.io/epipredict/reference/layer_population_scaling.md)
  : Convert per-capita predictions to raw scale

- [`layer_predict()`](https://cmu-delphi.github.io/epipredict/reference/layer_predict.md)
  : Prediction layer for post-processing

- [`layer_predictive_distn()`](https://cmu-delphi.github.io/epipredict/reference/layer_predictive_distn.md)
  **\[deprecated\]** : Returns predictive distributions

- [`layer_quantile_distn()`](https://cmu-delphi.github.io/epipredict/reference/layer_quantile_distn.md)
  : Returns predictive quantiles

- [`layer_residual_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/layer_residual_quantiles.md)
  : Creates predictions based on residual quantiles

- [`layer_threshold()`](https://cmu-delphi.github.io/epipredict/reference/layer_threshold.md)
  : Lower and upper thresholds for predicted values

- [`layer_unnest()`](https://cmu-delphi.github.io/epipredict/reference/layer_unnest.md)
  : Unnest prediction list-cols

## Epiworkflows

### Basic forecasting workflow functions

- [`epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/epi_recipe.md)
  : Create a epi_recipe for preprocessing data

- [`epi_workflow()`](https://cmu-delphi.github.io/epipredict/reference/epi_workflow.md)
  : Create an epi_workflow

- [`add_epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/add_epi_recipe.md)
  [`remove_epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/add_epi_recipe.md)
  [`update_epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/add_epi_recipe.md)
  :

  Add/remove/update the `epi_recipe` of an `epi_workflow`

- [`fit(`*`<epi_workflow>`*`)`](https://cmu-delphi.github.io/epipredict/reference/fit-epi_workflow.md)
  :

  Fit an `epi_workflow` object

### Forecast post-processing workflow functions

Create and apply series of post-processing operations

- [`frosting()`](https://cmu-delphi.github.io/epipredict/reference/frosting.md)
  : Create frosting for post-processing predictions

- [`add_frosting()`](https://cmu-delphi.github.io/epipredict/reference/add_frosting.md)
  [`remove_frosting()`](https://cmu-delphi.github.io/epipredict/reference/add_frosting.md)
  [`update_frosting()`](https://cmu-delphi.github.io/epipredict/reference/add_frosting.md)
  :

  Add/remove/update the `frosting` of an `epi_workflow`

- [`adjust_frosting()`](https://cmu-delphi.github.io/epipredict/reference/adjust_frosting.md)
  :

  Adjust a layer in an `epi_workflow` or `frosting`

- [`apply_frosting()`](https://cmu-delphi.github.io/epipredict/reference/apply_frosting.md)
  : Apply post-processing to a fitted workflow

- [`extract_frosting()`](https://cmu-delphi.github.io/epipredict/reference/extract_frosting.md)
  : Extract the frosting object from a workflow

- [`tidy(`*`<frosting>`*`)`](https://cmu-delphi.github.io/epipredict/reference/tidy.frosting.md)
  : Tidy the result of a frosting object

- [`slather()`](https://cmu-delphi.github.io/epipredict/reference/slather.md)
  : Spread a layer of frosting on a fitted workflow

### Prediction

Methods for prediction and modifying predictions

- [`predict(`*`<epi_workflow>`*`)`](https://cmu-delphi.github.io/epipredict/reference/predict-epi_workflow.md)
  : Predict from an epi_workflow
- [`augment(`*`<epi_workflow>`*`)`](https://cmu-delphi.github.io/epipredict/reference/augment.epi_workflow.md)
  : Augment data with predictions
- [`get_test_data()`](https://cmu-delphi.github.io/epipredict/reference/get_test_data.md)
  : Get test data for prediction based on longest lag period
- [`forecast(`*`<epi_workflow>`*`)`](https://cmu-delphi.github.io/epipredict/reference/forecast.epi_workflow.md)
  : Produce a forecast from an epi workflow and it's training data

### Modifying forecasting epiworkflows

Modify or inspect an existing recipe, workflow, or frosting. See also
[the article on the
topic](https://cmu-delphi.github.io/epipredict/articles/update.md)

- [`adjust_epi_recipe()`](https://cmu-delphi.github.io/epipredict/reference/adjust_epi_recipe.md)
  :

  Adjust a step in an `epi_workflow` or `epi_recipe`

- [`Add_model()`](https://cmu-delphi.github.io/epipredict/reference/Add_model.md)
  [`Remove_model()`](https://cmu-delphi.github.io/epipredict/reference/Add_model.md)
  [`Update_model()`](https://cmu-delphi.github.io/epipredict/reference/Add_model.md)
  [`add_model()`](https://cmu-delphi.github.io/epipredict/reference/Add_model.md)
  [`remove_model()`](https://cmu-delphi.github.io/epipredict/reference/Add_model.md)
  [`update_model()`](https://cmu-delphi.github.io/epipredict/reference/Add_model.md)
  :

  Add a model to an `epi_workflow`

- [`add_layer()`](https://cmu-delphi.github.io/epipredict/reference/add_layer.md)
  : Add layer to a frosting object

- [`extract_layers()`](https://cmu-delphi.github.io/epipredict/reference/layer-processors.md)
  [`is_layer()`](https://cmu-delphi.github.io/epipredict/reference/layer-processors.md)
  [`validate_layer()`](https://cmu-delphi.github.io/epipredict/reference/layer-processors.md)
  [`detect_layer()`](https://cmu-delphi.github.io/epipredict/reference/layer-processors.md)
  : Extract, validate, or detect layers of frosting

- [`update(`*`<layer>`*`)`](https://cmu-delphi.github.io/epipredict/reference/update.layer.md)
  :

  Update post-processing `layer`

## Automatic forecast visualization

- [`autoplot(`*`<epi_workflow>`*`)`](https://cmu-delphi.github.io/epipredict/reference/autoplot-epipred.md)
  [`autoplot(`*`<canned_epipred>`*`)`](https://cmu-delphi.github.io/epipredict/reference/autoplot-epipred.md)
  [`plot(`*`<epi_workflow>`*`)`](https://cmu-delphi.github.io/epipredict/reference/autoplot-epipred.md)
  [`plot(`*`<canned_epipred>`*`)`](https://cmu-delphi.github.io/epipredict/reference/autoplot-epipred.md)
  :

  Automatically plot an `epi_workflow` or `canned_epipred` object

## Parsnip engines

Prediction methods not available in the [general parsnip
repository](https://www.tidymodels.org/find/parsnip/)

- [`quantile_reg()`](https://cmu-delphi.github.io/epipredict/reference/quantile_reg.md)
  : Quantile regression
- [`smooth_quantile_reg()`](https://cmu-delphi.github.io/epipredict/reference/smooth_quantile_reg.md)
  : Smooth quantile regression
- [`grf_quantiles`](https://cmu-delphi.github.io/epipredict/reference/grf_quantiles.md)
  : Random quantile forests via grf

## Utilities

- [`flusight_hub_formatter()`](https://cmu-delphi.github.io/epipredict/reference/flusight_hub_formatter.md)
  : Format predictions for submission to FluSight forecast Hub
- [`clean_f_name()`](https://cmu-delphi.github.io/epipredict/reference/clean_f_name.md)
  : Create short function names
- [`check_enough_data()`](https://cmu-delphi.github.io/epipredict/reference/check_enough_data.md)
  : Check the dataset contains enough data points.

### Utilities for quantile distribution processing

- [`pivot_quantiles_longer()`](https://cmu-delphi.github.io/epipredict/reference/pivot_quantiles.md)
  [`pivot_quantiles_wider()`](https://cmu-delphi.github.io/epipredict/reference/pivot_quantiles.md)
  :

  Pivot a column containing `quantile_pred` to explicit rows or columns

- [`dist_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/dist_quantiles.md)
  **\[deprecated\]** : A distribution parameterized by a set of
  quantiles

- [`quantile(`*`<quantile_pred>`*`)`](https://cmu-delphi.github.io/epipredict/reference/quantile.quantile_pred.md)
  : Quantiles from a distribution

- [`extrapolate_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/extrapolate_quantiles.md)
  : Extrapolate the quantiles to new quantile levels

- [`nested_quantiles()`](https://cmu-delphi.github.io/epipredict/reference/nested_quantiles.md)
  **\[deprecated\]** : Turn a vector of quantile distributions into a
  list-col

- [`weighted_interval_score()`](https://cmu-delphi.github.io/epipredict/reference/weighted_interval_score.md)
  : Compute weighted interval score
