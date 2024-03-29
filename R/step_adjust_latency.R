#' adapt the pipeline to latency in the data
#'
#' In the standard case, the pipeline assumes that the last observation is also
#' the day from which the forecast is being made. `step_adjust_latency` uses the
#' `as_of` date of the `epi_df` as the `forecast_date`. This is most useful in
#' realtime and pseudo-prospective forecasting for data where there is some
#' delay between the day recorded and when that data is available.
#'
#' @param recipe A recipe object. The step will be added to the
#'  sequence of operations for this recipe.
#' @param ... One or more selector functions to choose variables for this step.
#'   See [recipes::selections()] for more details. Typically you will not need
#'   to set this manually, as the necessary adjustments will be done for the
#'   predictors and outcome.
#' @param method a character. Determines the method by which the
#'   forecast handles latency. All of these assume the forecast date is the
#'   `as_of` of the `epi_df`. The options are:
#'   - `"extend_ahead"`: Lengthen the ahead so that forecasting from the last
#'   observation results in a forecast `ahead` after the `as_of` date. E.g. if
#'   there are 3 days of latency between the last observation and the `as_of`
#'   date for a 4 day ahead forecast, the ahead used in practice is actually 7.
#'   - `"locf"`: carries forward the last observed value(s) up to the forecast
#'   date. See the Vignette TODO for equivalents using other steps and more
#'   sophisticated methods of extrapolation.
#'   - `"extend_lags"`: per `epi_key` and `predictor`, adjusts the lag so that
#'   the shortest lag at predict time is at the last observation. E.g. if the
#'   lags are `c(0,7,14)` for data that is 3 days latent, the actual lags used
#'   become `c(3,10,17)`
#' @param default Determines what fills empty rows
#'   left by leading/lagging (defaults to NA).
#' @param prefix a character. The prefix matching the one used in either
#'   `step_epi_ahead` if `method="extend_ahead"` or `step_epi_lag`
#'   if `method="extend_lags"` or "locf".
#' @param skip A logical. Should the step be skipped when the
#'  recipe is baked by [bake()]? While all operations are baked
#'  when [prep()] is run, some operations may not be able to be
#'  conducted on new data (e.g. processing the outcome variable(s)).
#'  Care should be taken when using `skip = TRUE` as it may affect
#'  the computations for subsequent operations.
#' @param id A unique identifier for the step
#' @template step-return
#'
#' @details The step assumes that the pipeline has already applied either
#'   `step_epi_ahead` or `step_epi_lag` depending on the value of
#'   `"method"`, and that `step_epi_naomit` has NOT been run.
#'
#' The `prefix` and `id` arguments are unchangeable to ensure that the code runs
#' properly and to avoid inconsistency with naming. For `step_epi_ahead`, they
#' are always set to `"ahead_"` and `"epi_ahead"` respectively, while for
#' `step_epi_lag`, they are set to `"lag_"` and `"epi_lag`, respectively.
#'
#' @family row operation steps
#' @rdname step_adjust_latency
#' @export
#' @examples
#' r <- epi_recipe(case_death_rate_subset) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   #   step_adjust_latency(method = "extend_ahead") %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14))
#' r
step_adjust_latency <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           method = c(
             "extend_ahead",
             "locf",
             "extend_lags"
           ),
           default = NA,
           skip = FALSE,
           prefix = NULL,
           columns = NULL,
           id = recipes::rand_id("epi_lag")) {
    if (!is_epi_recipe(recipe)) {
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")
    }
    if (!is.null(columns)) {
      rlang::abort(c("The `columns` argument must be `NULL.",
        i = "Use `tidyselect` methods to choose columns to lag."
      ))
    }

    method <- rlang::arg_match(method)
    if (method == "extend_ahead") {
      prefix <- "ahead_"
    } else {
      prefix <- "lag_"
    }

    arg_is_chr_scalar(prefix, id, method)
    recipes::add_step(
      recipe,
      step_adjust_latency_new(
        terms = dplyr::enquos(...),
        role = role,
        method = method,
        info = NULL,
        trained = trained,
        prefix = prefix,
        default = default,
        keys = epi_keys(recipe),
        columns = columns,
        skip = skip,
        id = id
      )
    )
  }

step_adjust_latency_new <-
  function(terms, role, trained, prefix, default, keys, method, info,
           columns, skip, id) {
    step(
      subclass = "adjust_latency",
      terms = terms,
      role = role,
      method = method,
      info = info,
      trained = trained,
      prefix = prefix,
      default = default,
      keys = keys,
      columns = columns,
      skip = skip,
      id = id
    )
  }

#' @export
prep.step_adjust_latency <- function(x, training, info = NULL, ...) {
  if ((x$method == "extend_ahead") && (!("outcome" %in% info$role))) {
    cli::cli_abort(glue::glue(c('If `method` is `"extend_ahead"`, then a step ",
          "must have already added an outcome .')))
  } else if (!("predictor" %in% info$role)) {
    cli::cli_abort('If `method` is `"extend_lags"` or `"locf"`, then a step ",
"must have already added a predictor.')
  }
  # TODO info here is probably not the best way to handle this, hypothetically I
  # get an info object during baking
  step_adjust_latency_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    prefix = x$prefix,
    default = x$default,
    keys = x$keys,
    method = x$method,
    info = info,
    columns = recipes::recipes_eval_select(x$terms, training, info),
    skip = x$skip,
    id = x$id
  )
}

#' various ways of handling differences between the `as_of` date and the maximum
#' time value
#' @description
#' adjust the ahead so that we will be predicting `ahead` days after the `as_of`
#'   date, rather than relative to the last day of data
#' @param new_data assumes that this already has lag/ahead columns that we need
#'   to adjust
#' @importFrom dplyr %>%
#' @keywords internal
#' @importFrom dplyr %>% pull
bake.step_adjust_latency <- function(object, new_data, ...) {
  sign_shift <- get_sign(object)
  # get the columns used, even if it's all of them
  terms_used <- object$columns
  if (length(terms_used) == 0) {
    terms_used <- object$info %>%
      filter(role == "raw") %>%
      pull(variable)
  }
  # get and check the max_time and as_of are the right kinds of dates
  as_of <- get_asof(object, new_data)

  # infer the correct columns to be working with from the previous
  # transformations
  shift_cols <- get_shifted_column_tibble(
    object, new_data, terms_used, as_of,
    sign_shift
  )

  if ((object$method == "extend_ahead") || (object$method == "extend_lags")) {
    # check that the shift amount isn't too extreme
    latency <- max(shift_cols$latency)
    i_latency <- which.max(shift_cols$latency)
    time_type <- attributes(new_data)$metadata$time_type
    if (
      (grepl("day", time_type) && (latency >= 10)) ||
        (grepl("week", time_type) && (latency >= 4)) ||
        ((time_type == "yearmonth") && (latency >= 2)) ||
        ((time_type == "yearquarter") && (latency >= 1)) ||
        ((time_type == "year") && (latency >= 1))
    ) {
      cli::cli_warn(c(
        "!" = glue::glue(
          "The shift has been adjusted by {latency}, ",
          "which is questionable for it's `time_type` of ",
          "{time_type}"
        ),
        "i" = "input ahead: {shift_cols$shifts[[i_latency]]}",
        "i" = "shifted ahead: {shift_cols$effective_shift[[i_latency]]}",
        "i" = "max_time = {max_time} -> as_of = {as_of}"
      ))
    }
    keys <- object$keys
    return(
      extend_either(new_data, shift_cols, keys)
    )
  }
}
