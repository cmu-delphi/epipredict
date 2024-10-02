# logical

    Code
      arg_is_lgl(l, ll, n)
    Condition
      Error:
      ! `n` must be of type <logical>.

---

    Code
      arg_is_lgl(x)
    Condition
      Error:
      ! `x` must be of type <logical>.

---

    Code
      arg_is_lgl(l, ll, nn)
    Condition
      Error:
      ! `nn` must be of type <logical>.

# scalar

    Code
      arg_is_scalar(x, y, n)
    Condition
      Error:
      ! `n` must be a scalar.

---

    Code
      arg_is_scalar(x, y, nn)
    Condition
      Error:
      ! `nn` must be a scalar.

---

    Code
      arg_is_scalar(v, nn)
    Condition
      Error:
      ! `v` must be a scalar.

---

    Code
      arg_is_scalar(v, nn, allow_na = TRUE)
    Condition
      Error:
      ! `v` must be a scalar.

---

    Code
      arg_is_scalar(v, n, allow_null = TRUE)
    Condition
      Error:
      ! `v` must be a scalar.

---

    Code
      arg_is_scalar(nnn, allow_na = TRUE)
    Condition
      Error:
      ! `nnn` must be a scalar.

# numeric

    Code
      arg_is_numeric(a)
    Condition
      Error:
      ! `a` must be of type <numeric>.

---

    Code
      arg_is_numeric(i, j, n)
    Condition
      Error:
      ! `n` must be of type <numeric>.

---

    Code
      arg_is_numeric(i, nn)
    Condition
      Error:
      ! `nn` must be of type <numeric>.

# positive

    Code
      arg_is_pos(a)
    Condition
      Error:
      ! `a` must be a strictly positive number.

---

    Code
      arg_is_pos(i, k)
    Condition
      Error:
      ! `k` must be strictly positive numbers.

---

    Code
      arg_is_pos(i, j, n)
    Condition
      Error:
      ! `n` must be strictly positive numbers.

---

    Code
      arg_is_pos(i, nn)
    Condition
      Error:
      ! `nn` must be a strictly positive number.

---

    Code
      arg_is_pos(a = 0:10)
    Condition
      Error:
      ! `0:10` must be strictly positive numbers.

# nonneg

    Code
      arg_is_nonneg(a)
    Condition
      Error:
      ! `a` must be a non-negative number.

---

    Code
      arg_is_nonneg(i, k)
    Condition
      Error:
      ! `k` must be non-negative numbers.

---

    Code
      arg_is_nonneg(i, j, n)
    Condition
      Error:
      ! `n` must be non-negative numbers.

---

    Code
      arg_is_nonneg(i, nn)
    Condition
      Error:
      ! `nn` must be a non-negative number.

# nonneg-int

    Code
      arg_is_nonneg_int(a)
    Condition
      Error:
      ! `a` must be a non-negative integer.

---

    Code
      arg_is_nonneg_int(d)
    Condition
      Error:
      ! `d` must be a non-negative integer.

---

    Code
      arg_is_nonneg_int(i, k)
    Condition
      Error:
      ! `k` must be non-negative integers.

---

    Code
      arg_is_nonneg_int(i, j, n)
    Condition
      Error:
      ! `n` must be non-negative integers.

---

    Code
      arg_is_nonneg_int(i, nn)
    Condition
      Error:
      ! `nn` must be a non-negative integer.

# date

    Code
      arg_is_date(d, dd, n)
    Condition
      Error:
      ! `n` must be dates.

---

    Code
      arg_is_date(d, dd, nn)
    Condition
      Error:
      ! `nn` must be a date.

---

    Code
      arg_is_date(a)
    Condition
      Error:
      ! `a` must be a date.

---

    Code
      arg_is_date(v)
    Condition
      Error:
      ! `v` must be dates.

---

    Code
      arg_is_date(ll)
    Condition
      Error:
      ! `ll` must be dates.

# probabilities

    Code
      arg_is_probabilities(a)
    Condition
      Error:
      ! `a` must lie in [0, 1].

---

    Code
      arg_is_probabilities(d)
    Condition
      Error:
      ! `d` must lie in [0, 1].

---

    Code
      arg_is_probabilities(i, 1.1)
    Condition
      Error:
      ! `1.1` must lie in [0, 1].

---

    Code
      arg_is_probabilities(c(0.4, 0.8), n)
    Condition
      Error:
      ! `n` must lie in [0, 1].

---

    Code
      arg_is_probabilities(c(0.4, 0.8), nn)
    Condition
      Error:
      ! `nn` must lie in [0, 1].

# chr

    Code
      arg_is_chr(a, b, n)
    Condition
      Error:
      ! `n` must be of type <character>.

---

    Code
      arg_is_chr(a, b, nn)
    Condition
      Error:
      ! `nn` must be of type <character>.

---

    Code
      arg_is_chr(d)
    Condition
      Error:
      ! `d` must be of type <character>.

---

    Code
      arg_is_chr(v)
    Condition
      Error:
      ! `v` must be of type <character>.

---

    Code
      arg_is_chr(ll)
    Condition
      Error:
      ! `ll` must be of type <character>.

---

    Code
      arg_is_chr(z)
    Condition
      Error:
      ! `z` must be of type <character>.

# function

    Code
      arg_is_function(c(a, b))
    Condition
      Error:
      ! `c(a, b)` must be of type <function>.

---

    Code
      arg_is_function(c(f, g))
    Condition
      Error:
      ! `c(f, g)` must be of type <function>.

---

    Code
      arg_is_function(f)
    Condition
      Error:
      ! `f` must be of type <function>.

# coerce scalar to date

    Code
      arg_to_date("12345")
    Condition
      Error in `arg_to_date()`:
      ! `x` must be a date.

---

    Code
      arg_to_date(c("12345", "12345"))
    Condition
      Error in `arg_to_date()`:
      ! `x` must be a scalar.

# simple surface step test

    Code
      recipe(jhu_csse_daily_subset) %>% step_epi_lag(death_rate, lag = "hello")
    Condition
      Error in `step_epi_lag()`:
      ! `lag` must be a non-negative integer.

