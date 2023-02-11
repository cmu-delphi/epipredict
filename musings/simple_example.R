# remotes::install_github("cmu-delphi/epipredict")
library(epipredict)
library(ggplot2)
library(dplyr)
library(tidyr)
library(recipes)
library(parsnip)
library(workflows)
dat <- case_death_rate_subset %>% # 1 year of daily data, 56 locations
  filter(time_value >= "2021-11-01", geo_value %in% c("ca", "ny", "pa"))
dat

# Now, 3 states for 61 days as a "long" data frame
# This data happens to be "regular" since it was revised 5 months later.
# But it typically would not be. For example, some states didn't report on
# Christmas / New Years, those values becoming available only much later.

# Very simple task:
# Predict `death_rate` at h = 2 weeks after the last available time_value
# Use lags of `case_rate` and `death_rate` as features
# We'll use
#   death_rate lags of 0 (today), 7, 14 days
#   case_rate lags of 0, 1, 2, 3, 7, 14 days
# We also want uncertainty bands around the forecasts.
# A "simple", nonparametric version uses quantiles of the training residuals
# (Yes, this is problematic for many reasons, but isn't obviously terrible
# in practice. And it illustrates post-processing.)

r <- epi_recipe(dat) %>%
  step_epi_lag(case_rate, lag = c(0, 1, 2, 3, 7, 14)) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_ahead(death_rate, ahead = 14) %>%
  recipes::step_naomit(all_predictors(), skip = FALSE) %>%
  recipes::step_naomit(all_outcomes(), skip = TRUE)

# epi_recipe knows how to handle the time_value and geo_value, and keeps them
# around to label predictions eventually.
#
# step_epi_* performs lag/lead via modifications to the time_value and
# join operations rather than shifting the vectors and "hoping"
r

# time_value and geo_value are always given roles, any additional "key"
# variables will be given the role="key". Data columns are assigned the role
# "raw" since they are unlikely to be predictors/outcomes by default
r %>% prep() %>% bake(dat)

# our post-processor, taking the prep/bake analogy to its logical extreme
# it's possible you would do something to the model before making predictions
# so we add a prediction layer
f <- frosting() %>%
  layer_predict() %>%
  layer_residual_quantiles(probs = c(.1, .9), symmetrize = TRUE) %>%
  layer_threshold(starts_with(".pred"), lower = 0) %>%
  # predictions/intervals should be non-negative
  layer_add_target_date(target_date = max(dat$time_value) + 14)
f


ewf <- epi_workflow(r, linear_reg(), f)
ewf
trained_ewf <- ewf %>% fit(dat)

# examines the recipe to determine what data is required to make the prediction
# Note: it should NOT be affected by the leading step, or we'll lose
# valuable recent data
latest <- get_test_data(r, dat)
preds <- trained_ewf %>% predict(new_data = latest)
preds

# just for fun, we examine these forecasts
ggplot(dat, aes(colour = geo_value)) +
  geom_line(aes(time_value, death_rate)) +
  geom_point(data = preds, aes(x = target_date, y = .pred)) +
  geom_errorbar(
    data = preds %>%
      mutate(q = nested_quantiles(.pred_distn)) %>%
      unnest(q) %>%
      pivot_wider(names_from = tau, values_from = q),
    aes(x = target_date, ymin = `0.1`, ymax = `0.9`)
  ) +
  theme_bw()

sessionInfo()
