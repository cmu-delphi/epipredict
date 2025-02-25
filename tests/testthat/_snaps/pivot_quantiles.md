# quantile pivotting wider behaves

    Code
      pivot_quantiles_wider(tib, a)
    Condition
      Error in `pivot_quantiles_wider()`:
      ! `a` is not <`quantile_pred`>. Cannot pivot it.

---

    Code
      pivot_quantiles_wider(tib, d1, d2)
    Condition
      Error in `pivot_quantiles_wider()`:
      ! Only one column can be pivotted. Can not pivot all of: `d1` and `d2`.

---

    Code
      pivot_quantiles_longer(tib, d1, d2)
    Condition
      Error in `pivot_quantiles_longer()`:
      ! Only one column can be pivotted. Can not pivot all of: `d1` and `d2`.

# quantile pivotting longer behaves

    Code
      pivot_quantiles_longer(tib, a)
    Condition
      Error in `pivot_quantiles_longer()`:
      ! `a` is not <`quantile_pred`>. Cannot pivot it.

---

    Code
      pivot_quantiles_longer(tib, d1, d2)
    Condition
      Error in `pivot_quantiles_longer()`:
      ! Only one column can be pivotted. Can not pivot all of: `d1` and `d2`.

# nested_quantiles is deprecated, but works where possible

    Code
      d <- dist_quantiles(list(1:4, 2:5), 1:4 / 5)
    Condition
      Warning:
      `dist_quantiles()` was deprecated in epipredict 0.1.11.
      i Please use `hardhat::quantile_pred()` instead.

---

    Code
      o <- nested_quantiles(d)
    Condition
      Warning:
      `nested_quantiles()` was deprecated in epipredict 0.1.11.
      i Please use `hardhat::quantile_pred()` instead.

