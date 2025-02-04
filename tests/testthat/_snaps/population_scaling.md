# expect error if `by` selector does not match

    Code
      wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu) %>% add_frosting(f)
    Condition
      Error in `hardhat::validate_column_names()`:
      ! The required column "a" is missing.

---

    Code
      forecast(wf)
    Condition
      Error in `hardhat::validate_column_names()`:
      ! The required column "nothere" is missing.

