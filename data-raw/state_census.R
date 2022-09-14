library(dplyr)
library(tidyr)

state_census <- covidcast::state_census %>%
  select(STATE, NAME, POPESTIMATE2019, ABBR) %>%
  rename(abbr = ABBR, name = NAME, pop = POPESTIMATE2019, fips = STATE) %>%
  mutate(abbr = tolower(abbr)) %>%
  as_tibble()


usethis::use_data(state_census, overwrite = TRUE)
