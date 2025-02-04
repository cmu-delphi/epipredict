# epi_recipe produces error if not an epi_df

    Code
      epi_recipe(tib)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'epi_recipe' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

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
      Error in `UseMethod()`:
      ! no applicable method for 'epi_recipe' applied to an object of class "c('matrix', 'array', 'character')"

# add/update/adjust/remove epi_recipe works as intended

    Code
      workflows::extract_preprocessor(wf)$steps
    Condition
      Error in `workflows::extract_preprocessor()`:
      ! The workflow does not have a preprocessor.

