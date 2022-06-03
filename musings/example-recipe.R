library(tidyverse)
library(covidcast)
library(delphi.epidata)
library(epiprocess)
library(tidymodels)

x <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200301, 20211231),
  geo_values = "*"
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, case_rate = value)

y <- covidcast(
  data_source = "jhu-csse",
  signals = "deaths_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20200301, 20211231),
  geo_values = "*"
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, death_rate = value)

x <- x %>%
  full_join(y, by = c("geo_value", "time_value")) %>%
  as_epi_df()
rm(y)

xx <- x %>% filter(time_value > "2021-12-01")


# Baseline AR3
r <- epi_recipe(x) %>% # if we add this as a class, maybe we get better
                       # behaviour downstream?
  step_epi_ahead(death_rate, ahead = 7) %>%
  step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
  step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
  step_narm()


slm <- linear_reg()

# slm_fit <- workflow(r, slm)
slm_fit <- workflow() %>%
  add_recipe(r) %>%
  add_model(slm) %>%
  fit(data = x)

x_latest <- x %>%
  filter(!is.na(case_rate), !is.na(death_rate)) %>%
  group_by(geo_value) %>%
  slice_tail(n = 15) # have lag 0,...,14, so need 15 for a complete case

pp <- predict(slm_fit, new_data = x_latest) # drops the keys...

xl <- x %>%
  filter(!is.na(case_rate), !is.na(death_rate)) %>%
  group_by(geo_value) %>%
  slice_tail(n = 14)
