path_to_csvs <- here::here("../../COVID-BC/Covid19Canada/updates.nosync/")
files <- list.files(path_to_csvs)
ca_as_ofs <- as.Date(substr(files, 1, 10)) %>%
  intersect(fc_time_values) %>%
  as.Date(origin = "1970-01-01")

can <- purrr::map(ca_as_ofs, ~ {
  readr::read_csv(here::here(path_to_csvs, paste0(.x, ".csv"))) %>%
    left_join(ca_pop) %>%
    mutate(time_value = lubridate::dmy(date_report)) %>%
    filter(province %in% ca_pop$province, time_value > "2020-04-01") %>%
    mutate(geo_value = province,
           case_rate = cases / population * 1e5) %>%
    select(geo_value, time_value, case_rate) %>%
    as_epi_df(geo_type = "province", as_of = .x)
})
names(can) <- ca_as_ofs
can <- can %>% bind_rows(.id = "version") %>%
  mutate(version = lubridate::ymd(version))
saveRDS(can, "inst/extdata/can_prov_cases.rds")
