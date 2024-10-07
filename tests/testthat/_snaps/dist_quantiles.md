# constructor returns reasonable quantiles

    Code
      new_quantiles(rnorm(5), c(-2, -1, 0, 1, 2))
    Condition
      Error in `new_quantiles()`:
      ! `quantile_levels` must lie in [0, 1].

---

    Code
      new_quantiles(sort(rnorm(5)), sort(runif(2)))
    Condition
      Error in `new_quantiles()`:
      ! length(values) == length(quantile_levels) is not TRUE

---

    Code
      new_quantiles(c(2, 1, 3, 4, 5), c(0.1, 0.1, 0.2, 0.5, 0.8))
    Condition
      Error in `new_quantiles()`:
      ! !vctrs::vec_duplicate_any(quantile_levels) is not TRUE

---

    Code
      new_quantiles(c(2, 1, 3, 4, 5), c(0.1, 0.15, 0.2, 0.5, 0.8))
    Output
      quantiles(4)[5]

---

    Code
      new_quantiles(c(1, 2, 3), c(0.1, 0.2, 3))
    Condition
      Error in `new_quantiles()`:
      ! `quantile_levels` must lie in [0, 1].

# arithmetic works on quantiles

    Code
      sum(dstn)
    Condition
      Error in `mapply()`:
      ! You can't perform arithmetic between two distributions like this.

---

    Code
      suppressWarnings(dstn + distributional::dist_normal())
    Condition
      Error:
      ! non-numeric argument to binary operator

