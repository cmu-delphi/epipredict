# A layer can be updated in frosting

    Code
      update(f$layers[[1]], lower = 100)
    Condition
      Error in `recipes:::update_fields()`:
      ! The step you are trying to update, `layer_predict()`, does not have the lower field.

---

    Code
      update(f$layers[[3]], lower = 100)
    Condition
      Error in `f$layers[[3]]`:
      ! subscript out of bounds

---

    Code
      update(f$layers[[2]], bad_param = 100)
    Condition
      Error in `recipes:::update_fields()`:
      ! The step you are trying to update, `layer_threshold()`, does not have the bad_param field.

