# quantile pivotting wider behaves

    Code
      pivot_quantiles_wider(tib, a)
    Condition
<<<<<<< HEAD
      Error in `pivot_quantiles_wider()`:
      ! `a` is not <`quantile_pred`>. Cannot pivot it.
=======
      Error in `UseMethod()`:
      ! no applicable method for 'family' applied to an object of class "c('integer', 'numeric')"
>>>>>>> main

---

    Code
<<<<<<< HEAD
      pivot_quantiles_wider(tib, d1, d2)
    Condition
      Error in `pivot_quantiles_wider()`:
      ! Only one column can be pivotted. Can not pivot all of: `d1` and `d2`.
=======
      pivot_quantiles_wider(tib, c)
    Condition
      Error in `validate_pivot_quantiles()`:
      ! Variables(s) `c` are not `dist_quantiles`. Cannot pivot them.
>>>>>>> main

---

    Code
<<<<<<< HEAD
      pivot_quantiles_longer(tib, d1, d2)
    Condition
      Error in `pivot_quantiles_longer()`:
      ! Only one column can be pivotted. Can not pivot all of: `d1` and `d2`.
=======
      pivot_quantiles_wider(tib, d1)
    Condition
      Error in `pivot_quantiles_wider()`:
      ! Quantiles must be the same length and have the same set of taus.
      i Check failed for variables(s) `d1`.
>>>>>>> main

# quantile pivotting longer behaves

    Code
      pivot_quantiles_longer(tib, a)
    Condition
<<<<<<< HEAD
      Error in `pivot_quantiles_longer()`:
      ! `a` is not <`quantile_pred`>. Cannot pivot it.
=======
      Error in `UseMethod()`:
      ! no applicable method for 'family' applied to an object of class "c('integer', 'numeric')"
>>>>>>> main

---

    Code
<<<<<<< HEAD
      pivot_quantiles_longer(tib, d1, d2)
    Condition
      Error in `pivot_quantiles_longer()`:
      ! Only one column can be pivotted. Can not pivot all of: `d1` and `d2`.
=======
      pivot_quantiles_longer(tib, c)
    Condition
      Error in `validate_pivot_quantiles()`:
      ! Variables(s) `c` are not `dist_quantiles`. Cannot pivot them.

---

    Code
      pivot_quantiles_longer(tib, d1, d3)
    Condition
      Error in `pivot_quantiles_longer()`:
      ! Some selected columns contain different numbers of quantiles.
      The result would be a very long <tibble>.
      To do this anyway, rerun with `.ignore_length_check = TRUE`.
>>>>>>> main

