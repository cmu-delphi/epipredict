library(cansim)
library(dplyr)
library(tidyr)

# Get statcan data using get_cansim, which returns a tibble
gemploy <- get_cansim("37-10-0115-01")

gemploy <- gemploy %>%
  # Drop some columns and rename the ones we keep
  select(c(
    "REF_DATE", "GEO", "VALUE", "STATUS", "Educational qualification",
    "Field of study", "Gender", "Age group", "Status of student in Canada",
    "Characteristics after graduation", "Graduate statistics"
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
  # The original `VALUE` column contain the statistic indicated by
  # `Graduate statistics` in the original data. Below we pivot the data
  # wider so that each unique statistic can have its own column.
  mutate(
    # Recode for easier pivoting
    grad_stat = recode_factor(
      grad_stat,
      `Number of graduates` = "num_graduates",
      `Median employment income two years after graduation` = "med_income_2y",
      `Median employment income five years after graduation` = "med_income_5y"
    ),
    # They are originally strings but want ints for conversion to epi_df later
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

saveRDS(gemploy, "vignettes/gemploy.rds")
