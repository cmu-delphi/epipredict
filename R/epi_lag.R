#' Create a lagged predictor
#'
#' `step_epi_lag` creates a *specification* of a recipe step that
#'   will add new columns of lagged data. Lagged data will
#'   by default include NA values where the lag was induced.
#'   These can be removed with [step_naomit()], or you may
#'   specify an alternative filler value with the `default`
#'   argument.
#'
#' @param lag A vector of positive integers. Each specified column will be
#'  lagged for each value in the vector.
#' @template step-return
#'
#' @details The step assumes that the data are already _in the proper sequential
#'  order_ for lagging.
#'
#' @family row operation steps
#' @export
#' @rdname step_epi_ahead
step_epi_lag <-
  function(recipe,
           ...,
           role = "predictor",
           trained = FALSE,
           lag = 1,
           prefix = "lag_",
           default = NA,
           keys = epi_keys(recipe),
           columns = NULL,
           skip = FALSE,
           id = rand_id("epi_lag")) {
    step_epi_shift(recipe,
                   ...,
                   role = role,
                   trained = trained,
                   lag = lag,
                   prefix = prefix,
                   default = default,
                   keys = keys,
                   columns = columns,
                   skip = skip,
                   id = id
    )
  }
