# epiwindow function works

    Code
      epiwindow(t0, time_value, 1.5)
    Condition
      Error in `epiwindow()`:
      ! Assertion on 'left' failed: Must be of type 'integerish', but element 1 is not close to an integer.

---

    Code
      epiwindow(t0, time_value, 1, 1.5)
    Condition
      Error in `epiwindow()`:
      ! Assertion on 'right' failed: Must be of type 'integerish', but element 1 is not close to an integer.

---

    Code
      epiwindow(t0, time_value, seasons = "other")
    Condition
      Error in `epiwindow()`:
      ! `seasons` must be one of "all" or "current", not "other".

---

    Code
      epiwindow(c(t0, t0), time_value)
    Condition
      Error in `epiwindow()`:
      ! Can't recycle `t0` (size 2) to match `time_value` (size 4).

