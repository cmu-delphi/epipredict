# arx_class_args checks inputs

    Code
      arx_class_args_list(ahead = c(0, 4))
    Condition
      Error in `arx_class_args_list()`:
      ! `ahead` must be a scalar.

---

    Code
      arx_class_args_list(n_training = c(28, 65))
    Condition
      Error in `arx_class_args_list()`:
      ! `n_training` must be a scalar.

---

    Code
      arx_class_args_list(ahead = -1)
    Condition
      Error in `arx_class_args_list()`:
      ! `ahead` must be a non-negative integer.

---

    Code
      arx_class_args_list(ahead = 1.5)
    Condition
      Error in `arx_class_args_list()`:
      ! `ahead` must be a non-negative integer.

---

    Code
      arx_class_args_list(n_training = -1)
    Condition
      Error in `arx_class_args_list()`:
      ! `n_training` must be a strictly positive number.

---

    Code
      arx_class_args_list(n_training = 1.5)
    Condition
      Error in `arx_class_args_list()`:
      ! `n_training` must be a positive integer.

---

    Code
      arx_class_args_list(lags = c(-1, 0))
    Condition
      Error in `arx_class_args_list()`:
      ! `lags` must be non-negative integers.

---

    Code
      arx_class_args_list(lags = list(c(1:5, 6.5), 2:8))
    Condition
      Error in `arx_class_args_list()`:
      ! `lags` must be non-negative integers.

---

    Code
      arx_class_args_list(target_date = "2022-01-01")
    Condition
      Error in `arx_class_args_list()`:
      ! `target_date` must be a date.

---

    Code
      arx_class_args_list(n_training_min = "de")
    Condition
      Error in `arx_class_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * n_training_min = "de"

---

    Code
      arx_class_args_list(epi_keys = 1)
    Condition
      Error in `arx_class_args_list()`:
      ! `...` must be empty.
      x Problematic argument:
      * epi_keys = 1

