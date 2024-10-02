# wis dispatches and produces the correct values

    Code
      weighted_interval_score(1:10, 10)
    Condition
      Error in `weighted_interval_score()`:
      ! Weighted interval score can only be calculated if `x` has class <quantile_pred>.

---

    Code
      weighted_interval_score(quantile_pred(rbind(1:4, 8:11), 1:4 / 5), 1:3)
    Condition
      Error in `weighted_interval_score.quantile_pred()`:
      ! Assertion on 'actual' failed: Must have length 2, but has length 3.

