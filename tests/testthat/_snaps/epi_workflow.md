# model can be added/updated/removed from epi_workflow

    Code
      extract_spec_parsnip(wf)
    Condition
      Error in `extract_spec_parsnip()`:
      ! The workflow does not have a model spec.

# forecast method errors when workflow not fit

    Code
      forecast(wf)
    Condition
      Error in `forecast()`:
      ! You cannot `forecast()` a <workflow> that has not been trained.
      i Please use `fit()` before forecasting.

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

