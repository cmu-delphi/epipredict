# Values for ahead and lag must be integer values

    Code
      r1 <- epi_recipe(x) %>% step_epi_ahead(death_rate, ahead = 3.6) %>%
        step_epi_lag(death_rate, lag = 1.9)
    Condition
      Error in `step_epi_ahead()`:
      ! `ahead` must be a non-negative integer.

# A negative lag value should should throw an error

    Code
      r2 <- epi_recipe(x) %>% step_epi_ahead(death_rate, ahead = 7) %>% step_epi_lag(
        death_rate, lag = -7)
    Condition
      Error in `step_epi_lag()`:
      ! `lag` must be a non-negative integer.

# Values for ahead and lag cannot be duplicates

    Code
      slm_fit(r4)
    Condition
      Error in `add_shifted_columns()`:
      ! Name collision occured in <step_epi_lag>
      The following variable name already exists: "lag_7_death_rate".

