# fit method does not silently drop the class

    epi_recipe has been called with a non-epi_df object, returning a regular recipe. Various step_epi_* functions will not work.

---

    Code
      ewf_erec_edf %>% fit(tbl)
    Condition
      Error in `if (new_meta != old_meta) ...`:
      ! argument is of length zero

