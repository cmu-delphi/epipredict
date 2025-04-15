# Yeo-Johnson transformation inverts correctly

    Code
      yj_transform(x, c(1, 2, 3))
    Condition
      Error:
      ! Length of `x` must be equal to length of `lambda`.

---

    Code
      yj_transform(list(1, 2), c(1, 2, 3))
    Condition
      Error:
      ! Length of `x` must be equal to length of `lambda`.

