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
    "Graduate statistics")) %>%
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
    "grad_stat" = "Graduate statistics") %>%
  mutate(
    grad_stat = recode_factor(
      grad_stat, 
      `Number of graduates` = "num_graduates", 
      `Median employment income two years after graduation` = "med_income_2y",
      `Median employment income five years after graduation` = "med_income_5y"),
    time_value = as.integer(time_value)
  ) %>%
  pivot_wider(names_from = grad_stat, values_from = value) %>%
  filter(
    # Drop aggregates for some columns
    geo_value != "Canada" & 
    age_group != "15 to 64 years" &
<<<<<<< HEAD
    fos != "Total, field of study" &
    edu_qual != "Total, educational qualification" &
    # Keep aggregates for keys we don't want to keep
=======
    edu_qual != "Total, educational qualification" &
    # Keep aggregates for keys we don't want to keep
    fos == "Total, field of study" &
>>>>>>> d04e2b43c614c4209e754d267d003b60521ff715
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
    !is.na(num_graduates) & !is.na(med_income_2y) & !is.na(med_income_5y)) %>%
<<<<<<< HEAD
  select(-c(status, gender, student_status, grad_charac))

# gemploy$time_value %>% unique()
# class(gemploy$fos)
# class(gemploy$edu_qual)
# class(gemploy$age_group)
# head(gemploy)
nrow(gemploy)
ncol(gemploy)

gemploy$grad_charac %>% unique()
gemploy %>% group_by(grad_charac) %>% slice(1)

grad_employ_subset <- gemploy %>%
  tsibble::as_tsibble(
    index=time_value, 
    key=c(geo_value, age_group, fos, edu_qual)) %>%
  as_epi_df(
    geo_type = "custom", time_type = "year", as_of = "2022-07-19",
    additional_metadata=c(other_keys=list("age_group", "fos", "edu_qual")))
usethis::use_data(grad_employ_subset, overwrite = TRUE)

# ================== EDA ==================

statcan_grad_employ %>% 
  group_by(`Characteristics after graduation`) %>% 
  # filter(`Graduate statistics` == "Median employment income two years after graduation") %>%
  filter(`Graduate statistics` == "Number of graduates") %>%
  slice(1) %>% 
  select(`Characteristics after graduation`)

names(statcan_grad_employ)
unique(statcan_grad_employ$REF_DATE)
# [1] "2010" "2011" "2012" "2013" "2014" "2015"

unique(statcan_grad_employ$GEO)
#  [1] "Canada"                    "Newfoundland and Labrador"
#  [3] "Prince Edward Island"      "Nova Scotia"              
#  [5] "New Brunswick"             "Quebec"                   
#  [7] "Ontario"                   "Manitoba"                 
#  [9] "Saskatchewan"              "Alberta"                  
# [11] "British Columbia"          "Territories"  
unique(statcan_grad_employ$DGUID)

unique(statcan_grad_employ$UOM)
# [1] "Number"                "2020 constant dollars"
statcan_grad_employ %>% 
  group_by(UOM, `Graduate statistics`) %>% 
  select(UOM, `Graduate statistics`, VALUE) %>% 
  slice(1)
unique(statcan_grad_employ$UOM_ID)

unique(statcan_grad_employ$SCALAR_FACTOR) # "units"
unique(statcan_grad_employ$SCALAR_ID) # "0"

length(unique(statcan_grad_employ$VECTOR))
length(unique(statcan_grad_employ$COORDINATE))

length(unique(statcan_grad_employ$VALUE))
unique(statcan_grad_employ$STATUS)
# [1] NA   ".." "x" 

unique(statcan_grad_employ$SYMBOL)
# [1] NA

unique(statcan_grad_employ$TERMINATED)
# [1] NA

unique(statcan_grad_employ$DECIMALS)
# [1] "0"

unique(statcan_grad_employ$GeoUID)
# redundant with geo

unique(statcan_grad_employ$`Hierarchy for GEO`)
# redundant with geo

unique(statcan_grad_employ$`Classification Code for Educational qualification`) # NA
unique(statcan_grad_employ$`Hierarchy for Educational qualification`)

unique(statcan_grad_employ$`Classification Code for Field of study`)
unique(statcan_grad_employ$`Hierarchy for Field of study`)

unique(statcan_grad_employ$`Classification Code for Gender`) # NA
unique(statcan_grad_employ$`Hierarchy for Gender`)

unique(statcan_grad_employ$`Classification Code for Age group`) # NA
unique(statcan_grad_employ$`Hierarchy for Age group`)

unique(statcan_grad_employ$`Classification Code for Status of student in Canada`) # NA
unique(statcan_grad_employ$`Hierarchy for Status of student in Canada`)

unique(statcan_grad_employ$`Classification Code for Characteristics after graduation`) # NA
unique(statcan_grad_employ$`Hierarchy for Characteristics after graduation`)

unique(statcan_grad_employ$`Classification Code for Graduate statistics`) # NA
unique(statcan_grad_employ$`Hierarchy for Graduate statistics`)

length(unique(statcan_grad_employ$`val_norm`))
statcan_grad_employ %>% filter(val_norm != VALUE) %>% nrow() #0

unique(statcan_grad_employ$`Date`)
# [1] "2010-07-01" "2011-07-01" "2012-07-01" "2013-07-01" "2014-07-01"
# [6] "2015-07-01"

unique(statcan_grad_employ$`Educational qualification`) # 32 levels
unique(statcan_grad_employ$`Field of study`) # 60 levels
unique(statcan_grad_employ$`Gender`)
unique(statcan_grad_employ$`Age group`)
unique(statcan_grad_employ$`Status of student in Canada`)
unique(statcan_grad_employ$`Characteristics after graduation`)
unique(statcan_grad_employ$`Graduate statistics`)
=======
  select(-c(status, gender, student_status, grad_charac, fos))

nrow(gemploy)
ncol(gemploy)

grad_employ_subset <- gemploy %>%
  tsibble::as_tsibble(
    index=time_value, 
    key=c(geo_value, age_group, edu_qual)) %>%
  as_epi_df(
    geo_type = "custom", time_type = "year", as_of = "2022-07-19",
    additional_metadata = list(other_keys = c("age_group", "edu_qual")))
usethis::use_data(grad_employ_subset, overwrite = TRUE)
>>>>>>> d04e2b43c614c4209e754d267d003b60521ff715
