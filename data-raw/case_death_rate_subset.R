library(tidyverse)
library(covidcast)
library(epidatr)
library(epiprocess)

x <- covidcast(
  data_source = "jhu-csse",
  signals = "confirmed_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20201231, 20211231),
  geo_values = "*"
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, case_rate = value)

y <- covidcast(
  data_source = "jhu-csse",
  signals = "deaths_7dav_incidence_prop",
  time_type = "day",
  geo_type = "state",
  time_values = epirange(20201231, 20211231),
  geo_values = "*"
) %>%
  fetch_tbl() %>%
  select(geo_value, time_value, death_rate = value)

case_death_rate_subset <- x %>%
  full_join(y, by = c("geo_value", "time_value")) %>%
  as_epi_df()

usethis::use_data(case_death_rate_subset, overwrite = TRUE)
