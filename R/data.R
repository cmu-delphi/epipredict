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
