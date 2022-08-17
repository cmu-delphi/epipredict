#' Subset of JHU daily state cases and deaths
#'
#' This data source of confirmed COVID-19 cases and deaths
#' is based on reports made available by the Center for
#' Systems Science and Engineering at Johns Hopkins University.
#' This example data ranges from Dec 31, 2020 to Dec 31, 2021,
#' and includes all states.
#'
#' @format A tibble with 20,496 rows and 4 variables:
#' \describe{
#'   \item{geo_value}{the geographic value associated with each row
#'       of measurements.}
#'   \item{time_value}{the time value associated with each row of measurements.}
#'   \item{case_rate}{7-day average signal of number of new
#'       confirmed COVID-19 cases per 100,000 population, daily}
#'   \item{death_rate}{7-day average signal of number of new confirmed
#'       deaths due to COVID-19 per 100,000 population, daily}
#' }
#' @source This object contains a modified part of the
#'   \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University}
#'   as \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{republished in the COVIDcast Epidata API}.
#'   This data set is licensed under the terms of the
#'   \href{https://creativecommons.org/licenses/by/4.0/}{Creative Commons Attribution 4.0 International license}
#'   by the Johns Hopkins University on behalf of its Center for Systems Science
#'   in Engineering. Copyright Johns Hopkins University 2020.
#'
#' Modifications:
#' * \href{https://cmu-delphi.github.io/delphi-epidata/api/covidcast-signals/jhu-csse.html}{From the COVIDcast Epidata API}:
#'   These signals are taken directly from the JHU CSSE
#'   \href{https://github.com/CSSEGISandData/COVID-19}{COVID-19 GitHub repository}
#'   without changes. The 7-day average signals are computed by Delphi by
#'   calculating moving averages of the preceding 7 days, so the signal for
#'   June 7 is the average of the underlying data for June 1 through 7,
#'   inclusive.
"case_death_rate_subset"

#' State population data
#'
#' Data set on state populations, from the 2019 US Census.
#'
#' @format Data frame with 57 rows (including one for the United States as a
#'   whole, plus the District of Columbia, Puerto Rico Commonwealth,
#'   American Samoa, Guam, the U.S. Virgin Islands, and the Northern Mariana,
#'   Islands).
#'
#' \describe{
#'   \item{fips}{FIPS code}
#'   \item{name}{Full name of the state or territory}
#'   \item{pop}{Estimate of the location's resident population in
#'      2019.}
#'   \item{abbr}{Postal abbreviation for the location}
#' }
#'
#' @source United States Census Bureau, at
#'   \url{https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.pdf},
#'   \url{https://www.census.gov/data/tables/time-series/demo/popest/2010s-total-puerto-rico-municipios.html},
#'   and \url{https://www.census.gov/data/tables/2010/dec/2010-island-areas.html}
"state_census"

#' Subset of Statistics Canada employment numbers by industry and province
#' 
#' @format A tibble with 109,388 rows and 6 variables:
#' \describe{
#'   \item{geo_value}{The province in Canada associated with each 
#'      row of measurements.}
#'   \item{time_value}{The time value, in YYYY-MM-01 format, 
#'      associated with each row of measurements.}
#'   \item{ppl_count}{The number of people employed, seasonally 
#'      adjusted.}
#'   \item{employee_type}{The type of employee}
#'   \item{naics_industry}{The industry name and associated code 
#'      according to \href{https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=1181553}{NAICS}}
#' }
#' @source This object contains modified data from the following Statistics Canada
#' data table: \href{https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1410022001#data}{Table 14-10-0220-01  Employment and average weekly earnings (including overtime) for all employees by industry, monthly, seasonally adjusted, Canada}
#' 
#' Modifications:
#' * From the given Statistics Canada table, the employee counts
#'   are taken as-is. Only \href{https://www23.statcan.gc.ca/imdb/p3VD.pl?Function=getVD&TVD=1181553}{NAICS} codes at hierarchy level 2 are 
#'   filtered in. Only data rows that are \href{https://www.statcan.gc.ca/en/concepts/definitions/guide-symbol}{good quality or higher and not missing}
#'   according to Statistics Canada are removed.
"statcan_employ_subset"

#' Subset of Statistics Canada median employment income for postsecondary graduates
#' 
#' @format A tibble with 10193 rows and 8 variables:
#' \describe{
#'   \item{geo_value}{The province in Canada associated with each 
#'      row of measurements.}
#'   \item{time_value}{The time value, a year integer in YYYY format}
#'   \item{edu_qual}{The education qualification}
#'   \item{fos}{The field of study}
#'   \item{age_group}{The age group; either 15 to 34 or 35 to 64}
#'   \item{num_graduates}{The number of graduates for the given row of characteristics}
#'   \item{med_income_2y}{The median employment income two years after graduation}
#'   \item{med_income_5y}{The median employment income five years after graduation}
#' }
#' @source This object contains modified data from the following Statistics Canada 
#' data table: \href{https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710011501}
#' 
#' Modifications:
#' * Only provincial-level geo_values are kept
#' * Only age group, field of study, and educational qualification are kept as 
#'   covariates. For the remaining covariates, we keep aggregated values and 
#'   drop the level-specific rows.
#' * No modifications were made to the time range of the data
"grad_employ_subset"
