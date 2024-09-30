# epi_recipe produces error if not an epi_df

    Code
      epi_recipe(tib)
    Condition
      Error in `epi_recipe()`:
      ! `x` must be an <epi_df> or a <formula>, not a <tbl_df>.

---

    Code
      epi_recipe(y ~ x, tib)
    Condition
      Error in `epi_recipe()`:
      ! `epi_recipe()` has been called with a non-<epi_df> object. Use `recipe()` instead.

---

    Code
      epi_recipe(m)
    Condition
      Error in `epi_recipe()`:
      ! `x` must be an <epi_df> or a <formula>, not a <matrix>.

