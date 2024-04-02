# test joining by default columns

    Code
      prep <- prep(r, jhu)
    Message
      Joining with `by = join_by(geo_value)`
      Joining with `by = join_by(geo_value)`

---

    Code
      b <- bake(prep, jhu)
    Message
      Joining with `by = join_by(geo_value)`
      Joining with `by = join_by(geo_value)`

---

    Code
      wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu) %>% add_frosting(f)
    Message
      Joining with `by = join_by(geo_value)`
      Joining with `by = join_by(geo_value)`

---

    Code
      p <- predict(wf, latest)
    Message
      Joining with `by = join_by(geo_value)`
      Joining with `by = join_by(geo_value)`

