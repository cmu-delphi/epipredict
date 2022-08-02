library(epipredict)
library(epiprocess)
library(cansim)
library(dplyr)
library(stringr)

# Run this once
# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410022001#data
statcan_employ <- get_cansim("14-10-0201-01")

# ================== Subset & Filtering ==================
employ <- statcan_employ %>%
    select(c(
        "REF_DATE",
        "GEO",
        # "DGUID",
        # "UOM",
        # "UOM_ID",
        # "SCALAR_FACTOR",
        # "SCALAR_ID",
        # "VECTOR",
        # "COORDINATE",
        "VALUE",
        "STATUS",
        # "SYMBOL",
        # "TERMINATED",
        # "DECIMALS",
        # "GeoUID",
        # "Hierarchy for GEO",
        # "Classification Code for Type of employee",
        # "Hierarchy for Type of employee",
        "Classification Code for North American Industry Classification System (NAICS)",
        # "Hierarchy for North American Industry Classification System (NAICS)",
        # "val_norm",
        # "Date",
        "Type of employee",
        "North American Industry Classification System (NAICS)")) %>%
    rename(
        "geo_value" = "GEO",
        "time_value" = "REF_DATE",
        "ppl_count" = "VALUE",
        "status" = "STATUS",
        "employee_type" = "Type of employee",
        "naics_industry" = "North American Industry Classification System (NAICS)",
        "naics_code" = "Classification Code for North American Industry Classification System (NAICS)"
    ) %>%
    mutate(time_value = tsibble::yearmonth(time_value, "%Y-%m")) %>%
    # https://www.statcan.gc.ca/en/concepts/definitions/guide-symbol
    # .. not available for a specific reference period
    # x: suppressed to meet the confidentiality requirements of the Statistics Act
    # A data quality: excellent
    # B data quality: very good
    # C data quality: good
    # [blank] or A-D: acceptable or better <- only keep these ones
    # E use with caution
    # F too unreliable to be published
    filter(
        status %in% c("A", "B", "C", "D", NA) & # only keep "good" data
        !is.na(ppl_count) &
        geo_value != "Canada" & # only keep provinces
        # N corresponds to aggregates
        !str_detect(naics_code, "N") &
        # only keep top-level sectors
        # https://www.census.gov/programs-surveys/economic-census/guidance/understanding-naics.html
        # corresponds to either [##] or [##-##]
        str_detect(naics_code, "(\\[[0-9]{2}\\])|(\\[[0-9]{2}-[0-9]{2}\\])") &
        # [00] corresponds to unclassified
        naics_code != "[00]") %>%
    select(-c(status, naics_code)) %>%
    # re-level the industry column because we dropped factors
    mutate(naics_industry = factor(naics_industry))

# head(employ)
# statcan_employ_subset <- employ %>%
#     tsibble::as_tsibble(
#         index=time_value, 
#         key=c(geo_value, employee_type, naics_industry)) %>%
#     as_epi_df(time_type = "yearmonth", as_of="2022-07-28")

statcan_employ_subset <- employ %>%
    tsibble::as_tsibble(index=time_value, key=c(geo_value, employee_type, naics_industry)) %>%
    as_epi_df(
        additional_metadata=c(other_keys=c("employee_type", "naics_industry")))

usethis::use_data(statcan_employ_subset, overwrite = TRUE)

# ================== EDA ==================
length(statcan_employ$REF_DATE)
names(statcan_employ)

uniq_ref_date <- unique(select(statcan_employ, "REF_DATE"))
uniq_ref_date
min(statcan_employ$REF_DATE) # 2001-01
max(statcan_employ$REF_DATE) # 2022-05
# There should be (22-1)*12 + 5
exp_total_dates <- (22-1)*12+5
length(uniq_ref_date %>% unlist()) == exp_total_dates # TRUE
# There is a ref date for each month in the date range

unique(select(statcan_employ, "GEO")) # List of length 14, names of provinces & territories + Canada
statcan_employ %>% group_by(GEO) %>% top_n(n=-1)
# Dissemination Geography Unique Identifier - DGUID.
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/Definition-eng.cfm?ID=geo055
# 2016 (year)
# A (administrative)
unique(select(statcan_employ, "DGUID"))
unique(select(statcan_employ, "UOM")) # Persons
unique(select(statcan_employ, "UOM_ID")) # 249

# These scalar columns tell us by what factor of 10 to multiply the values
# We get "units" and 0 so we can ignore these columns and
# take the values in VALUE as-is
unique(select(statcan_employ, "SCALAR_FACTOR")) # All "units"
unique(select(statcan_employ, "SCALAR_ID")) # All 0

# Time series identifier - ignore
unique(select(statcan_employ, "VECTOR"))

# Related to dimension, which is not present in this table - ignore
unique(select(statcan_employ, "COORDINATE"))

# Data value column. Values in units
unique(select(statcan_employ, "VALUE"))
# How many rows have empty values?
# Approx 3/4 of the rows have NA values
statcan_employ %>%
    summarise(
        VALUE_NA = sum(is.na(VALUE)) / length(VALUE),
        VALUE_NOT_NA = sum(!is.na(VALUE)) / length(VALUE),
        TOTAL = length(VALUE)
    )

unique(select(statcan_employ, "STATUS"))
statcan_employ %>%
    select(STATUS, VALUE) %>%
    group_by(STATUS) %>%
    count()

unique(select(statcan_employ, "SYMBOL")) # All NA
unique(select(statcan_employ, "TERMINATED")) # All NA
unique(select(statcan_employ, "DECIMALS")) # All 0

unique(select(statcan_employ, "GeoUID"))
unique(select(statcan_employ, "Hierarchy for GEO"))
statcan_employ %>%
    group_by_at(c("GEO", "DGUID", "GeoUID", "Hierarchy for GEO")) %>%
    count()
# These 4 columns are redundant. Just keep GEO.

# The next 4 columns are metadata about the last 2 columns
# ignore these in favour of the descriptive ones
unique(select(statcan_employ, "Classification Code for Type of employee")) # All NA
unique(select(statcan_employ, "Hierarchy for Type of employee"))
unique(select(statcan_employ, "Classification Code for North American Industry Classification System (NAICS)"))
unique(select(statcan_employ, "Hierarchy for North American Industry Classification System (NAICS)"))

# val_norm and VALUE are the same
unique(select(statcan_employ, "val_norm"))
statcan_employ %>% filter(VALUE != val_norm) %>% count()

unique(select(statcan_employ, "Date"))
# Each date has a minimum of 7522 data points
statcan_employ %>% group_by(Date) %>% count() %>% ungroup() %>% select(n) %>% min()
# Are there any dates that aren't on the 1st of the month?
statcan_employ %>% filter(format(as.Date(Date), "%d") != "01") %>% nrow() # 0

unique(select(statcan_employ, "Type of employee")) # 3 types
unique(select(statcan_employ, "North American Industry Classification System (NAICS)")) # lots

# REF_DATE looks like YYYY-mm
# Date looks like YYYY-mm-dd
# Check that the truncated Date to REF_DATE format always matches the REF_DATE
statcan_employ %>%
    select(REF_DATE, Date) %>%
    mutate(Date_trunc = format(as.Date(Date), "%Y-%m")) %>%
    filter(REF_DATE != Date_trunc) # all empty! good

# This is an example plot
# library(ggplot2)
# theme_set(theme_bw())
#
# employ <- statcan_employ_subset %>%
#     dplyr::filter(
#         geo_value %in% c("British Columbia", "Ontario") &
#             naics_industry == "Real estate and rental and leasing [53]") %>%
#     dplyr::arrange(geo_value, time_value)
#
# employ %>% ggplot(aes(x = time_value, y = ppl_count, color=employee_type)) +
#     geom_line() +
#     facet_wrap(vars(geo_value), scales = "free_y", ncol = 1) +
#     scale_x_date(minor_breaks = "month", date_labels = "%b %y") +
#     labs(x = "Date", y = "Number employed")
