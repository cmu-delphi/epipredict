# expect error if `by` selector does not match

    Code
      wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu) %>% add_frosting(f)
    Condition
      Error in `step_population_scaling()`:
      Caused by error in `hardhat::validate_column_names()`:
      ! The following required columns are missing: 'a'.

---

    Code
      forecast(wf)
    Condition
      Error in `hardhat::validate_column_names()`:
      ! The following required columns are missing: 'nothere'.

