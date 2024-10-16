# flatline_args_list checks inputs

    Code
      flatline_args_list(ahead = c(0, 4))
    Condition
      Error in `flatline_args_list()`:
      ! `ahead` must be a scalar.

---

    Code
      flatline_args_list(n_training = c(28, 65))
    Condition
      Error in `flatline_args_list()`:
      ! `n_training` must be a scalar.

---

    Code
      flatline_args_list(ahead = -1)
    Condition
      Error in `flatline_args_list()`:
      ! `ahead` must be a non-negative integer.

---

    Code
      flatline_args_list(ahead = 1.5)
    Condition
      Error in `flatline_args_list()`:
      ! `ahead` must be a non-negative integer.

---

    Code
      flatline_args_list(n_training = -1)
    Condition
      Error in `flatline_args_list()`:
      ! `n_training` must be a strictly positive number.

---

    Code
      flatline_args_list(n_training = 1.5)
    Condition
      Error in `flatline_args_list()`:
      ! `n_training` must be a positive integer.

---

    Code
      flatline_args_list(lags = c(-1, 0))
    Condition
      Error in `flatline_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * lags = c(-1, 0)

---

    Code
      flatline_args_list(lags = list(c(1:5, 6.5), 2:8))
    Condition
      Error in `flatline_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * lags = list(c(1:5, 6.5), 2:8)

---

    Code
      flatline_args_list(symmetrize = 4)
    Condition
      Error in `flatline_args_list()`:
      ! `symmetrize` must be of type <logical>.

---

    Code
      flatline_args_list(nonneg = 4)
    Condition
      Error in `flatline_args_list()`:
      ! `nonneg` must be of type <logical>.

---

    Code
      flatline_args_list(quantile_levels = -0.1)
    Condition
      Error in `flatline_args_list()`:
      ! `quantile_levels` must lie in [0, 1].

---

    Code
      flatline_args_list(quantile_levels = 1.1)
    Condition
      Error in `flatline_args_list()`:
      ! `quantile_levels` must lie in [0, 1].

---

    Code
      flatline_args_list(target_date = "2022-01-01")
    Condition
      Error in `flatline_args_list()`:
      ! `target_date` must be a date.

---

    Code
      flatline_args_list(n_training_min = "de")
    Condition
      Error in `flatline_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * n_training_min = "de"

---

    Code
      flatline_args_list(epi_keys = 1)
    Condition
      Error in `flatline_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * epi_keys = 1

