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
#' data table: \href{https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710011501}{
#'  Characteristics and median employment income of longitudinal cohorts of postsecondary 
#'  graduates two and five years after graduation, by educational qualification and 
#'  field of study (primary groupings)
#' }
#' 
#' Modifications:
#' * Only provincial-level geo_values are kept
#' * Only age group, field of study, and educational qualification are kept as 
#'   covariates. For the remaining covariates, we keep aggregated values and 
#'   drop the level-specific rows.
#' * No modifications were made to the time range of the data
"grad_employ_subset"