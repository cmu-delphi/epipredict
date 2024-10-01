#' Add a model to an `epi_workflow`
#'
#' @seealso [workflows::add_model()]
#' - `Add_model()` adds a parsnip model to the `epi_workflow`.
#'
#' - `Remove_model()` removes the model specification as well as any fitted
#'   model object. Any extra formulas are also removed.
#'
#' - `Update_model()` first removes the model then adds the new
#' specification to the workflow.
#'
#' @details
#' Has the same behaviour as [workflows::add_model()] but also ensures
#' that the returned object is an `epi_workflow`.
#'
#' This family is called `Add_*` / `Update_*` / `Remove_*` to avoid
#' masking the related functions in `{workflows}`. We also provide
#' aliases with the lower-case names. However, in the event that
#' `{workflows}` is loaded after `{epipredict}`, these may fail to function
#' properly.
#'
#' @inheritParams workflows::add_model
#'
#' @param x An `epi_workflow`.
#'
#' @param spec A parsnip model specification.
#'
#' @param ... Not used.
#'
#' @return
#' `x`, updated with a new, updated, or removed model.
#'
#' @export
#' @examples
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7)
#'
#' rf_model <- rand_forest(mode = "regression")
#'
#' wf <- epi_workflow(r)
#'
#' wf <- wf %>% Add_model(rf_model)
#' wf
#'
#' lm_model <- linear_reg()
#'
#' wf <- Update_model(wf, lm_model)
#' wf
#'
#' wf <- Remove_model(wf)
#' wf
#' @export
Add_model <- function(x, spec, ..., formula = NULL) {
  UseMethod("Add_model")
}

#' @rdname Add_model
#' @export
Remove_model <- function(x) {
  UseMethod("Remove_model")
}

#' @rdname Add_model
#' @export
Update_model <- function(x, spec, ..., formula = NULL) {
  UseMethod("Update_model")
}

#' @rdname Add_model
#' @export
Add_model.epi_workflow <- function(x, spec, ..., formula = NULL) {
  workflows::add_model(x, spec, ..., formula = formula)
}

#' @rdname Add_model
#' @export
Remove_model.epi_workflow <- function(x) {
  workflows:::validate_is_workflow(x)

  if (!workflows:::has_spec(x)) {
    rlang::warn("The workflow has no model to remove.")
  }

  new_epi_workflow(
    pre = x$pre,
    fit = workflows:::new_stage_fit(),
    post = x$post,
    trained = FALSE
  )
}

#' @rdname Add_model
#' @export
Update_model.epi_workflow <- function(x, spec, ..., formula = NULL) {
  rlang::check_dots_empty()
  x <- Remove_model(x)
  Add_model(x, spec, ..., formula = formula)
}


#' @rdname Add_model
#' @export
Add_model.workflow <- workflows::add_model

#' @rdname Add_model
#' @export
Remove_model.workflow <- workflows::remove_model

#' @rdname Add_model
#' @export
Update_model.workflow <- workflows::update_model


# Aliases -----------------------------------------------------------------

#' @rdname Add_model
#' @export
add_model <- Add_model

#' @rdname Add_model
#' @export
remove_model <- Remove_model

#' @rdname Add_model
#' @export
update_model <- Update_model
