library(epipredict)
library(epiprocess)
library(cansim)
library(dplyr)
library(stringr)
library(tidyr)

# https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710011501
statcan_grad_employ <- get_cansim("37-10-0115-01")

gemploy <- statcan_grad_employ %>%
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
    # "Classification Code for Educational qualification",
    # "Hierarchy for Educational qualification",
    # "Classification Code for Field of study",
    # "Hierarchy for Field of study",
    # "Classification Code for Gender",
    # "Hierarchy for Gender",
    # "Classification Code for Age group",
    # "Hierarchy for Age group",
    # "Classification Code for Status of student in Canada",
    # "Hierarchy for Status of student in Canada",
    # "Classification Code for Characteristics after graduation",
    # "Hierarchy for Characteristics after graduation",
    # "Classification Code for Graduate statistics",
    # "Hierarchy for Graduate statistics",
    # "val_norm",
    # "Date",
    "Educational qualification",
    "Field of study",
    "Gender",
    "Age group",
    "Status of student in Canada",
    "Characteristics after graduation",
    "Graduate statistics"
  )) %>%
  rename(
    "geo_value" = "GEO",
    "time_value" = "REF_DATE",
    "value" = "VALUE",
    "status" = "STATUS",
    "edu_qual" = "Educational qualification",
    "fos" = "Field of study",
    "gender" = "Gender",
    "age_group" = "Age group",
    "student_status" = "Status of student in Canada",
    "grad_charac" = "Characteristics after graduation",
    "grad_stat" = "Graduate statistics"
  ) %>%
  mutate(
    grad_stat = recode_factor(
      grad_stat,
      `Number of graduates` = "num_graduates",
      `Median employment income two years after graduation` = "med_income_2y",
      `Median employment income five years after graduation` = "med_income_5y"
    ),
    time_value = as.integer(time_value)
  ) %>%
  pivot_wider(names_from = grad_stat, values_from = value) %>%
  filter(
    # Drop aggregates for some columns
    geo_value != "Canada" &
      age_group != "15 to 64 years" &
      edu_qual != "Total, educational qualification" &
      # Keep aggregates for keys we don't want to keep
      fos == "Total, field of study" &
      gender == "Total, gender" &
      student_status == "Canadian and international students" &
      # Since we're looking at 2y and 5y employment income, the only
      # characteristics remaining are:
      # - Graduates reporting employment income
      # - Graduates reporting wages, salaries, and commissions only
      # For simplicity, keep the first one only
      grad_charac == "Graduates reporting employment income" &
      # Only keep "good" data
      is.na(status) &
      # Drop NA value rows
      !is.na(num_graduates) & !is.na(med_income_2y) & !is.na(med_income_5y)
  ) %>%
  select(-c(status, gender, student_status, grad_charac, fos))

nrow(gemploy)
ncol(gemploy)

grad_employ_subset <- gemploy %>%
  as_epi_df(
    as_of = "2022-07-19",
    additional_metadata = list(other_keys = c("age_group", "edu_qual"))
  )
usethis::use_data(grad_employ_subset, overwrite = TRUE)
