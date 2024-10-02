# arx_args checks inputs

    Code
      arx_args_list(ahead = c(0, 4))
    Condition
      Error in `arx_args_list()`:
      ! `ahead` must be a scalar.

---

    Code
      arx_args_list(n_training = c(28, 65))
    Condition
      Error in `arx_args_list()`:
      ! `n_training` must be a scalar.

---

    Code
      arx_args_list(ahead = -1)
    Condition
      Error in `arx_args_list()`:
      ! `ahead` must be a non-negative integer.

---

    Code
      arx_args_list(ahead = 1.5)
    Condition
      Error in `arx_args_list()`:
      ! `ahead` must be a non-negative integer.

---

    Code
      arx_args_list(n_training = -1)
    Condition
      Error in `arx_args_list()`:
      ! `n_training` must be a strictly positive number.

---

    Code
      arx_args_list(n_training = 1.5)
    Condition
      Error in `arx_args_list()`:
      ! `n_training` must be a positive integer.

---

    Code
      arx_args_list(lags = c(-1, 0))
    Condition
      Error in `arx_args_list()`:
      ! `lags` must be non-negative integers.

---

    Code
      arx_args_list(lags = list(c(1:5, 6.5), 2:8))
    Condition
      Error in `arx_args_list()`:
      ! `lags` must be non-negative integers.

---

    Code
      arx_args_list(symmetrize = 4)
    Condition
      Error in `arx_args_list()`:
      ! `symmetrize` must be of type <logical>.

---

    Code
      arx_args_list(nonneg = 4)
    Condition
      Error in `arx_args_list()`:
      ! `nonneg` must be of type <logical>.

---

    Code
      arx_args_list(quantile_levels = -0.1)
    Condition
      Error in `arx_args_list()`:
      ! `quantile_levels` must lie in [0, 1].

---

    Code
      arx_args_list(quantile_levels = 1.1)
    Condition
      Error in `arx_args_list()`:
      ! `quantile_levels` must lie in [0, 1].

---

    Code
      arx_args_list(target_date = "2022-01-01")
    Condition
      Error in `arx_args_list()`:
      ! `target_date` must be a date.

---

    Code
      arx_args_list(n_training_min = "de")
    Condition
      Error in `arx_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * n_training_min = "de"

---

    Code
      arx_args_list(epi_keys = 1)
    Condition
      Error in `arx_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * epi_keys = 1

# arx forecaster disambiguates quantiles

    Code
      compare_quantile_args(alist, tlist)
    Condition
      Error in `compare_quantile_args()`:
      ! You have specified different, non-default, quantiles in the trainier and `arx_args` options.
      i Please only specify quantiles in one location.

# arx_lags_validator handles named & unnamed lists as expected

    Code
      arx_lags_validator(pred_vec, lags_finit_fn_switch2)
    Condition
      Error in `arx_lags_validator()`:
      ! You have requested 2 predictor(s) but 3 different lags.
      i Lags must be a vector or a list with length == number of predictors.

---

    Code
      arx_lags_validator(pred_vec, lags_init_other_name)
    Condition
      Error in `arx_lags_validator()`:
      ! If lags is a named list, then all predictors must be present.
      i The predictors are `death_rate` and `case_rate`.
      i So lags is missing `case_rate`'.

