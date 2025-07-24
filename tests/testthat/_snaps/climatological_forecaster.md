# climate args list validates properly

    Code
      climate_args_list(forecast_date = 12345)
    Condition
      Error in `climate_args_list()`:
      ! `forecast_date` must be a date.

---

    Code
      climate_args_list(forecast_date = as.Date(c("2021-01-10", "2024-01-22")))
    Condition
      Error in `climate_args_list()`:
      ! `forecast_date` must be a scalar.

---

    Code
      climate_args_list(forecast_horizon = 1.3)
    Condition
      Error in `climate_args_list()`:
      ! `forecast_horizon` must be a integer.

---

    Code
      climate_args_list(window_size = -1)
    Condition
      Error in `climate_args_list()`:
      ! `window_size` must be a non-negative integer.

---

    Code
      climate_args_list(window_size = 2.5)
    Condition
      Error in `climate_args_list()`:
      ! `window_size` must be a non-negative integer.

---

    Code
      climate_args_list(window_size = 1:3)
    Condition
      Error in `climate_args_list()`:
      ! `window_size` must be a scalar.

---

    Code
      climate_args_list(quantile_levels = -1)
    Condition
      Error in `climate_args_list()`:
      ! `quantile_levels` must lie in [0, 1].

---

    Code
      climate_args_list(quantile_levels = 1.3)
    Condition
      Error in `climate_args_list()`:
      ! `quantile_levels` must lie in [0, 1].

---

    Code
      climate_args_list(symmetrize = 2.5)
    Condition
      Error in `climate_args_list()`:
      ! `symmetrize` must be of type <logical>.

---

    Code
      climate_args_list(symmetrize = c(TRUE, TRUE))
    Condition
      Error in `climate_args_list()`:
      ! `symmetrize` must be a scalar.

---

    Code
      climate_args_list(nonneg = 2.5)
    Condition
      Error in `climate_args_list()`:
      ! `nonneg` must be of type <logical>.

---

    Code
      climate_args_list(nonneg = c(TRUE, TRUE))
    Condition
      Error in `climate_args_list()`:
      ! `nonneg` must be a scalar.

---

    Code
      climate_args_list(quantile_by_key = TRUE)
    Condition
      Error in `climate_args_list()`:
      ! `quantile_by_key` must be of type <character>.

---

    Code
      climate_args_list(quantile_by_key = 2:3)
    Condition
      Error in `climate_args_list()`:
      ! `quantile_by_key` must be of type <character>.

