# epi_slide errors when needed

    Code
      recipe(edf) %>% step_epi_slide(value, .f = mean, .window_size = 7L)
    Condition
      Error in `step_epi_slide()`:
      ! This recipe step can only operate on an <epi_recipe>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = c(3L, 6L))
    Condition
      Error in `epiprocess:::validate_slide_window_arg()`:
      ! Slide function expected `.window_size` to be a length-1 difftime with units in days or non-negative integer or Inf.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .align = c("right", "left"))
    Condition
      Error in `step_epi_slide()`:
      ! step_epi_slide: `.window_size` must be specified.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, skip = c(TRUE, FALSE))
    Condition
      Error in `step_epi_slide()`:
      ! `skip` must be a scalar of type <logical>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, role = letters[1:2])
    Condition
      Error in `step_epi_slide()`:
      ! `role` must be a scalar of type <character>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, prefix = letters[1:2])
    Condition
      Error in `step_epi_slide()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, id = letters[1:2])
    Condition
      Error in `step_epi_slide()`:
      ! `id` must be a scalar of type <character>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1.5)
    Condition
      Error in `epiprocess:::validate_slide_window_arg()`:
      ! Slide function expected `.window_size` to be a length-1 difftime with units in days or non-negative integer or Inf.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, .align = 1.5)
    Condition
      Error in `step_epi_slide()`:
      ! `.align` must be a character vector, not the number 1.5.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, skip = "a")
    Condition
      Error in `step_epi_slide()`:
      ! `skip` must be a scalar of type <logical>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, role = 1)
    Condition
      Error in `step_epi_slide()`:
      ! `role` must be a scalar of type <character>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, prefix = 1)
    Condition
      Error in `step_epi_slide()`:
      ! `prefix` must be a scalar of type <character>.

---

    Code
      r %>% step_epi_slide(value, .f = mean, .window_size = 1L, id = 1)
    Condition
      Error in `step_epi_slide()`:
      ! `id` must be a scalar of type <character>.

---

    Code
      r %>% step_epi_slide(value)
    Condition
      Error in `rlang::is_formula()`:
      ! argument ".f" is missing, with no default

---

    Code
      r %>% step_epi_slide(value, .f = 1)
    Condition
      Error in `validate_slide_fun()`:
      ! In, `step_epi_slide()`, `.f` must be a function.

---

    Code
      r %>% step_epi_slide(value)
    Condition
      Error in `rlang::is_formula()`:
      ! argument ".f" is missing, with no default

---

    Code
      r %>% step_epi_slide(value, .f = 1)
    Condition
      Error in `validate_slide_fun()`:
      ! In, `step_epi_slide()`, `.f` must be a function.

# epi_slide handles different function specs

    Code
      lfun <- r %>% step_epi_slide(value, .f = ~ mean(.x, na.rm = TRUE),
      .window_size = 4L)
    Condition
      Error in `validate_slide_fun()`:
      ! In, `step_epi_slide()`, `.f` cannot be a formula.

