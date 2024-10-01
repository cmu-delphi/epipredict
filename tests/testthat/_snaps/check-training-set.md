# training set validation works

    Code
      validate_meta_match(t1, template, "geo_type", "abort")
    Condition
      Error in `validate_meta_match()`:
      ! The `geo_type` of the training data appears to be different from that
      used to construct the recipe. This may result in unexpected consequences.
      i Training `geo_type` is 'county'.
      i Originally, it was 'state'.

---

    Code
      epi_check_training_set(t4, rec)
    Condition
      Error in `epi_check_training_set()`:
      ! The recipe specifies keys which are not in the training data.
      i The training set is missing columns for missing_col.

