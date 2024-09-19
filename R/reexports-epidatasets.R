#' @inherit epidatasets::grad_employ_subset description source references title
#' @inheritSection epidatasets::grad_employ_subset Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epipredict::grad_employ_subset
#'
#' # works
#' library(epipredict)
#' grad_employ_subset
#'
#' # fails
#' data(grad_employ_subset, package = "epipredict")
#' @export
delayedAssign("grad_employ_subset", epidatasets::grad_employ_subset)

#' @inherit epidatasets::covid_case_death_rates description source references title
#' @inheritSection epidatasets::covid_case_death_rates Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epipredict::covid_case_death_rates
#'
#' # works
#' library(epipredict)
#' covid_case_death_rates
#'
#' # fails
#' data(covid_case_death_rates, package = "epipredict")
#' @export
delayedAssign("covid_case_death_rates", epidatasets::covid_case_death_rates)

#' @inherit epidatasets::state_census description source references title
#' @inheritSection epidatasets::state_census Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epipredict::state_census
#'
#' # works
#' library(epipredict)
#' state_census
#'
#' # fails
#' data(state_census, package = "epipredict")
#' @export
delayedAssign("state_census", epidatasets::state_census)

#' @inherit epidatasets::counts_subset description source references title
#' @inheritSection epidatasets::counts_subset Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epipredict::counts_subset
#'
#' # works
#' library(epipredict)
#' counts_subset
#'
#' # fails
#' data(counts_subset, package = "epipredict")
#' @export
delayedAssign("counts_subset", epidatasets::counts_subset)

#' @inherit epidatasets::ctis_covid_behaviours description source references title
#' @inheritSection epidatasets::ctis_covid_behaviours Data dictionary
#' @examples
#' # Since this is a re-exported dataset, it cannot be loaded using
#' # the `data()` function. `data()` looks for a file of the same name
#' # in the `data/` directory, which doesn't exist in this package.
#' # works
#' epipredict::ctis_covid_behaviours
#'
#' # works
#' library(epipredict)
#' ctis_covid_behaviours
#'
#' # fails
#' data(ctis_covid_behaviours, package = "epipredict")
#' @export
delayedAssign("ctis_covid_behaviours", epidatasets::ctis_covid_behaviours)
