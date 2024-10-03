library(dplyr)
library(tidyr)

remote_file <- "https://github.com/cmu-delphi/covidcast/raw/refs/heads/main/R-packages/covidcast/data/state_census.rda"
download.file(remote_file, "state_census.rda")
load("state_census.rda")
state_census <- state_census %>%
  select(STATE, NAME, POPESTIMATE2019, ABBR) %>%
  rename(abbr = ABBR, name = NAME, pop = POPESTIMATE2019, fips = STATE) %>%
  mutate(abbr = tolower(abbr)) %>%
  as_tibble()
file.remove("state_census.rda")

usethis::use_data(state_census, overwrite = TRUE)
