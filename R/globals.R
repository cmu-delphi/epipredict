# Handle "no visible binding for global variable" notes from R CMD check
# when using tidyverse/rlang pronouns and column names in NSE.
utils::globalVariables(c(
  ".", ".data", ".env", ".facets", ".idx", ".pred", ".pred_distn",
  ".pred_distn_all", ".pred_distn_quantile_level", ".pred_distn_value",
  ".pred_quantile", ".quantile_levels", ".resid", ".weights", "abbr",
  "ahead", "any_are_na", "cli_stop", "climate_pred", "col_name",
  "data", "distn", "fips", "forecast_date", "geo_value", "horizon", "i",
  "latency", "latency.bake", "latency.prep", "location", "matches",
  "median", "number", "object", "output_type_id", "reference_date", "role",
  "shift", "skip", "target_date", "target_end_date", "terms", "time_value",
  "type", "value", "variable"
))
