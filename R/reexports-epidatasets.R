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
