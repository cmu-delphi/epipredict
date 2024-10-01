# step_growth_rate validates arguments

    Code
      step_growth_rate(r)
    Condition
      Error in `step_growth_rate()`:
      ! This recipe step can only operate on an <epi_recipe>.

---

    Code
      step_growth_rate(r, value, role = 1)
    Condition
      Error in `step_growth_rate()`:
      ! `role` must be of type <character>.

---

    Code
      step_growth_rate(r, value, method = "abc")
    Condition
      Error in `step_growth_rate()`:
      ! `method` must be one of "rel_change" or "linear_reg", not "abc".

---

    Code
      step_growth_rate(r, value, horizon = 0)
    Condition
      Error in `step_growth_rate()`:
      ! `horizon` must be a positive integer.

---

    Code
      step_growth_rate(r, value, horizon = c(1, 2))
    Condition
      Error in `step_growth_rate()`:
      ! `horizon` must be a scalar.

---

    Code
      step_growth_rate(r, value, prefix = letters[1:2])
    Condition
      Error in `step_growth_rate()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      step_growth_rate(r, value, id = letters[1:2])
    Condition
      Error in `step_growth_rate()`:
      ! `id` must be a scalar of type <character>.

---

    Code
      step_growth_rate(r, value, prefix = letters[1:2])
    Condition
      Error in `step_growth_rate()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      step_growth_rate(r, value, prefix = 1)
    Condition
      Error in `step_growth_rate()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      step_growth_rate(r, value, id = 1)
    Condition
      Error in `step_growth_rate()`:
      ! `id` must be a scalar of type <character>.

---

    Code
      step_growth_rate(r, value, log_scale = 1)
    Condition
      Error in `step_growth_rate()`:
      ! `log_scale` must be a scalar of type <logical>.

---

    Code
      step_growth_rate(r, value, skip = 1)
    Condition
      Error in `step_growth_rate()`:
      ! `skip` must be a scalar of type <logical>.

---

    Code
      step_growth_rate(r, value, additional_gr_args_list = 1:5)
    Condition
      Error in `step_growth_rate()`:
      ! `additional_gr_args_list` must be a <list>.
      i See `?epiprocess::growth_rate` for available options.

---

    Code
      step_growth_rate(r, value, replace_Inf = "c")
    Condition
      Error in `step_growth_rate()`:
      ! `replace_Inf` must be of type <numeric>.

---

    Code
      step_growth_rate(r, value, replace_Inf = c(1, 2))
    Condition
      Error in `step_growth_rate()`:
      ! replace_Inf must be a scalar.

