# step_lag_difference validates arguments

    Code
      step_lag_difference(r)
    Condition
      Error in `step_lag_difference()`:
      ! This recipe step can only operate on an <epi_recipe>.

---

    Code
      step_lag_difference(r, value, role = 1)
    Condition
      Error in `step_lag_difference()`:
      ! `role` must be of type <character>.

---

    Code
      step_lag_difference(r, value, horizon = 0)
    Condition
      Error in `step_lag_difference()`:
      ! `horizon` must be a positive integer.

---

    Code
      step_lag_difference(r, value, prefix = letters[1:2])
    Condition
      Error in `step_lag_difference()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      step_lag_difference(r, value, id = letters[1:2])
    Condition
      Error in `step_lag_difference()`:
      ! `id` must be a scalar of type <character>.

---

    Code
      step_lag_difference(r, value, prefix = letters[1:2])
    Condition
      Error in `step_lag_difference()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      step_lag_difference(r, value, prefix = 1)
    Condition
      Error in `step_lag_difference()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      step_lag_difference(r, value, id = 1)
    Condition
      Error in `step_lag_difference()`:
      ! `id` must be a scalar of type <character>.

---

    Code
      step_lag_difference(r, value, skip = 1)
    Condition
      Error in `step_lag_difference()`:
      ! `skip` must be a scalar of type <logical>.

