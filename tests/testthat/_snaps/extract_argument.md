# layer argument extractor works

    Code
      extract_argument(f$layers[[1]], "uhoh", "bubble")
    Condition
      Error in `extract_argument()`:
      ! Requested "uhoh" not found. This is a(n) <layer_predict>.

---

    Code
      extract_argument(f$layers[[1]], "layer_predict", "bubble")
    Condition
      Error in `extract_argument()`:
      ! Requested argument "bubble" not found in "layer_predict".

---

    Code
      extract_argument(f, "layer_thresh", "quantile_levels")
    Condition
      Error in `extract_argument()`:
      ! frosting object does not contain a "layer_thresh".

---

    Code
      extract_argument(epi_workflow(), "layer_residual_quantiles", "quantile_levels")
    Condition
      Error in `extract_frosting()`:
      ! The epi_workflow does not have a postprocessor.

---

    Code
      extract_argument(wf, "layer_predict", c("type", "opts"))
    Condition
      Error in `FUN()`:
      ! `arg` must be a scalar of type <character>.

# recipe argument extractor works

    Code
      extract_argument(r$steps[[1]], "uhoh", "bubble")
    Condition
      Error in `extract_argument()`:
      ! Requested "uhoh" not found. This is a <step_epi_lag>.

---

    Code
      extract_argument(r$steps[[1]], "step_epi_lag", "bubble")
    Condition
      Error in `extract_argument()`:
      ! Requested argument "bubble" not found in "step_epi_lag".

---

    Code
      extract_argument(r, "step_lightly", "quantile_levels")
    Condition
      Error in `extract_argument()`:
      ! recipe object does not contain a "step_lightly".

---

    Code
      extract_argument(epi_workflow(), "step_epi_lag", "lag")
    Condition
      Error in `extract_argument()`:
      ! The workflow must have a recipe preprocessor.

