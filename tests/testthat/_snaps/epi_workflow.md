# fit method does not silently drop the class

    Code
      epi_recipe(y ~ x, data = tbl)
    Condition
      Error in `epi_recipe()`:
      ! `epi_recipe()` has been called with a non-<epi_df> object. Use `recipe()` instead.

---

    Code
      ewf_erec_edf %>% fit(tbl)
    Condition
      Error in `if (new_meta != old_meta) ...`:
      ! argument is of length zero

