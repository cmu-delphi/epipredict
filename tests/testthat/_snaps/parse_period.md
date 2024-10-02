# parse_period works

    Code
      parse_period(c(1, 2))
    Condition
      Error in `parse_period()`:
      ! `x` must be a scalar.

---

    Code
      parse_period(c(1.3))
    Condition
      Error in `parse_period()`:
      ! rlang::is_integerish(x) is not TRUE

---

    Code
      parse_period("1 year")
    Condition
      Error in `parse_period()`:
      ! incompatible timespan in `aheads`.

---

    Code
      parse_period("2 weeks later")
    Condition
      Error in `parse_period()`:
      ! incompatible timespan in `aheads`.

