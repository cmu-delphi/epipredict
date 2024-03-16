# test joining by default columns

    Code
      prep <- prep(r, jhu)

---

    Code
      b <- bake(prep, jhu)

---

    Code
      wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu) %>% add_frosting(f)

---

    Code
      p <- predict(wf, latest)

# expect error if `by` selector does not match

    Code
      wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu) %>% add_frosting(f)
    Condition
      Error in `hardhat::validate_column_names()`:
      ! The following required columns are missing: 'a'.

---

    Code
      forecast(wf)
    Condition
      Error in `hardhat::validate_column_names()`:
      ! The following required columns are missing: 'nothere'.

