# layer validation works

    Code
      layer_add_forecast_date(f, c("2022-05-31", "2022-05-31"))
    Condition
      Error in `layer_add_forecast_date()`:
      ! `forecast_date` must be a scalar.

---

    Code
      layer_add_forecast_date(f, "2022-05-31", id = 2)
    Condition
      Error in `layer_add_forecast_date()`:
      ! `id` must be a scalar of type <character>.

---

    Code
      layer_add_forecast_date(f, "2022-05-31", id = c("a", "b"))
    Condition
      Error in `layer_add_forecast_date()`:
      ! `id` must be a scalar of type <character>.

# forecast date works for daily

    Code
      predict(wf1, latest_yearly)
    Condition
      Error:
      ! Can't convert `data$time_value` <integer> to match type of `time_value` <date>.

---

    Code
      predict(wf3, latest)
    Condition
      Error in `layer_add_forecast_date()`:
      ! The `forecast_date` was given as a "year" while the
      ! `time_type` of the training data was "day".
      i See `?epiprocess::epi_df` for descriptions of these are determined.

