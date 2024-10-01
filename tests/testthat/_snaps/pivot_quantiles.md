# quantile pivotting wider behaves

    Code
      pivot_quantiles_wider(tib, a)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'family' applied to an object of class "c('integer', 'numeric')"

---

    Code
      pivot_quantiles_wider(tib, c)
    Condition
      Error in `validate_pivot_quantiles()`:
      ! Variables(s) `c` are not `dist_quantiles`. Cannot pivot them.

---

    Code
      pivot_quantiles_wider(tib, d1)
    Condition
      Error in `pivot_quantiles_wider()`:
      ! Quantiles must be the same length and have the same set of taus.
      i Check failed for variables(s) `d1`.

# quantile pivotting longer behaves

    Code
      pivot_quantiles_longer(tib, a)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'family' applied to an object of class "c('integer', 'numeric')"

---

    Code
      pivot_quantiles_longer(tib, c)
    Condition
      Error in `validate_pivot_quantiles()`:
      ! Variables(s) `c` are not `dist_quantiles`. Cannot pivot them.

---

    Code
      pivot_quantiles_longer(tib, d1, d3)
    Condition
      Error in `pivot_quantiles_longer()`:
      ! Some selected columns contain different numbers of quantiles. The result would be a very long <tibble>. To do this anyway, rerun with `.ignore_length_check = TRUE`.

