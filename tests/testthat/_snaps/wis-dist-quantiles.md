# wis dispatches and produces the correct values

    Code
      weighted_interval_score(1:10, 10)
    Condition
      Error in `weighted_interval_score()`:
      ! Weighted interval score can only be calculated if `x`
      has class <distribution>.

---

    Code
      weighted_interval_score(dist_quantiles(list(1:4, 8:11), 1:4 / 5), 1:3)
    Condition
      Error in `weighted_interval_score()`:
      ! Can't recycle `x` (size 2) to match `actual` (size 3).

