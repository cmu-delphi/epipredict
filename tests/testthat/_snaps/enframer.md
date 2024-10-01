# enframer errors/works as needed

    Code
      enframer(1:5, letters[1])
    Condition
      Error in `enframer()`:
      ! is.data.frame(df) is not TRUE

---

    Code
      enframer(data.frame(a = 1:5), 1:3)
    Condition
      Error in `enframer()`:
      ! `x` must be of type <character>.

---

    Code
      enframer(data.frame(a = 1:5), letters[1:3])
    Condition
      Error in `enframer()`:
      ! In enframer: some new cols match existing column names

---

    Code
      enframer(data.frame(aa = 1:5), letters[1:2], fill = 1:4)
    Condition
      Error in `enframer()`:
      ! length(fill) == 1 || length(fill) == nrow(df) is not TRUE

