library(dplyr)
library(tidyr)

state_census <- read_csv("https://github.com/cmu-delphi/covidcast/raw/c89e4d295550ba1540d64d2cc991badf63ad04e5/Python-packages/covidcast-py/covidcast/geo_mappings/state_census.csv") %>%
  select(STATE, NAME, POPESTIMATE2019, ABBR) %>%
  rename(abbr = ABBR, name = NAME, pop = POPESTIMATE2019, fips = STATE) %>%
  mutate(abbr = tolower(abbr)) %>%
  as_tibble()

usethis::use_data(state_census, overwrite = TRUE)
