#' Direct autoregressive classifier with covariates
#'
#' This is an autoregressive classification model for
#' [epiprocess::epi_df] data. It does "direct" forecasting, meaning
#' that it estimates a class at a particular target horizon.
#'
#' @inheritParams arx_forecaster
#' @param outcome A character (scalar) specifying the outcome (in the
#'   `epi_df`). Note that as with [arx_forecaster()], this is expected to
#'   be real-valued. Conversion of this data to unordered classes is handled
#'   internally based on the `breaks` argument to [arx_class_args_list()].
#'   If discrete classes are already in the `epi_df`, it is recommended to
#'   code up a classifier from scratch using [epi_recipe()].
#' @param trainer A `{parsnip}` model describing the type of estimation.
#'   For now, we enforce `mode = "classification"`. Typical values are
#'   [parsnip::logistic_reg()] or [parsnip::multinom_reg()]. More complicated
#'   trainers like [parsnip::naive_Bayes()] or [parsnip::rand_forest()] can
#'   also be used.
#' @param args_list A list of customization arguments to determine
#'   the type of forecasting model. See [arx_class_args_list()].
#'
#' @return A list with (1) `predictions` an `epi_df` of predicted classes
#'   and (2) `epi_workflow`, a list that encapsulates the entire estimation
#'   workflow
#' @export
#' @seealso [arx_class_epi_workflow()], [arx_class_args_list()]
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value >= as.Date("2021-11-01"))
#'
#' out <- arx_classifier(jhu, "death_rate", c("case_rate", "death_rate"))
#'
#' out <- arx_classifier(
#'   jhu,
#'   "death_rate",
#'   c("case_rate", "death_rate"),
#'   trainer = parsnip::multinom_reg(),
#'   args_list = arx_class_args_list(
#'     breaks = c(-.05, .1), ahead = 14,
#'     horizon = 14, method = "linear_reg"
#'   )
#' )
arx_classifier <- function(
    epi_data,
    outcome,
    predictors,
    trainer = parsnip::logistic_reg(),
    args_list = arx_class_args_list()) {
  if (!is_classification(trainer)) {
    cli::cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'classification'.")
  }

  wf <- arx_class_epi_workflow(
    epi_data, outcome, predictors, trainer, args_list
  )

  latest <- get_test_data(
    hardhat::extract_preprocessor(wf), epi_data, TRUE, args_list$nafill_buffer,
    args_list$forecast_date %||% max(epi_data$time_value)
  )

  wf <- generics::fit(wf, epi_data)
  preds <- predict(wf, new_data = latest) %>%
    tibble::as_tibble() %>%
    dplyr::select(-time_value)

  structure(
    list(
      predictions = preds,
      epi_workflow = wf,
      metadata = list(
        training = attr(epi_data, "metadata"),
        forecast_created = Sys.time()
      )
    ),
    class = c("arx_class", "canned_epipred")
  )
}


#' Create a template `arx_classifier` workflow
#'
#' This function creates an unfit workflow for use with [arx_classifier()].
#' It is useful if you want to make small modifications to that classifier
#' before fitting and predicting. Supplying a trainer to the function
#' may alter the returned `epi_workflow` object but can be omitted.
#'
#' @inheritParams arx_classifier
#' @param trainer A `{parsnip}` model describing the type of estimation.
#'  For now, we enforce `mode = "classification"`. Typical values are
#'  [parsnip::logistic_reg()] or [parsnip::multinom_reg()]. More complicated
#'  trainers like [parsnip::naive_Bayes()] or [parsnip::rand_forest()] can
#'  also be used. May be `NULL` (the default).
#'
#' @return An unfit `epi_workflow`.
#' @export
#' @seealso [arx_classifier()]
#' @examples
#'
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value >= as.Date("2021-11-01"))
#'
#' arx_class_epi_workflow(jhu, "death_rate", c("case_rate", "death_rate"))
#'
#' arx_class_epi_workflow(
#'   jhu,
#'   "death_rate",
#'   c("case_rate", "death_rate"),
#'   trainer = parsnip::multinom_reg(),
#'   args_list = arx_class_args_list(
#'     breaks = c(-.05, .1), ahead = 14,
#'     horizon = 14, method = "linear_reg"
#'   )
#' )
arx_class_epi_workflow <- function(
    epi_data,
    outcome,
    predictors,
    trainer = NULL,
    args_list = arx_class_args_list()) {
  validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_class", "alist"))) {
    rlang::abort("args_list was not created using `arx_class_args_list().")
  }
  if (!(is.null(trainer) || is_classification(trainer))) {
    rlang::abort("`trainer` must be a `{parsnip}` model of mode 'classification'.")
  }
  lags <- arx_lags_validator(predictors, args_list$lags)

  # --- preprocessor
  # ------- predictors
  r <- epi_recipe(epi_data) %>%
    step_growth_rate(
      tidyselect::all_of(predictors),
      role = "grp",
      horizon = args_list$horizon,
      method = args_list$method,
      log_scale = args_list$log_scale,
      additional_gr_args_list = args_list$additional_gr_args
    )
  for (l in seq_along(lags)) {
    p <- predictors[l]
    p <- as.character(glue::glue_data(args_list, "gr_{horizon}_{method}_{p}"))
    r <- step_epi_lag(r, !!p, lag = lags[[l]])
  }
  # ------- outcome
  if (args_list$outcome_transform == "lag_difference") {
    o <- as.character(
      glue::glue_data(args_list, "lag_diff_{horizon}_{outcome}")
    )
    r <- r %>%
      step_lag_difference(
        !!outcome,
        role = "pre-outcome",
        horizon = args_list$horizon
      )
  }
  if (args_list$outcome_transform == "growth_rate") {
    o <- as.character(
      glue::glue_data(args_list, "gr_{horizon}_{method}_{outcome}")
    )
    if (!(outcome %in% predictors)) {
      r <- r %>%
        step_growth_rate(
          !!outcome,
          role = "pre-outcome",
          horizon = args_list$horizon,
          method = args_list$method,
          log_scale = args_list$log_scale,
          additional_gr_args_list = args_list$additional_gr_args
        )
    }
  }
  o2 <- rlang::sym(paste0("ahead_", args_list$ahead, "_", o))
  r <- r %>%
    step_epi_ahead(!!o, ahead = args_list$ahead, role = "pre-outcome") %>%
    step_mutate(
      outcome_class = cut(!!o2, breaks = args_list$breaks),
      role = "outcome"
    ) %>%
    step_epi_naomit() %>%
    step_training_window(n_recent = args_list$n_training)

  forecast_date <- args_list$forecast_date %||% max(epi_data$time_value)
  target_date <- args_list$target_date %||% forecast_date + args_list$ahead

  # --- postprocessor
  f <- frosting() %>% layer_predict() # %>% layer_naomit()
  f <- layer_add_forecast_date(f, forecast_date = forecast_date) %>%
    layer_add_target_date(target_date = target_date)

  epi_workflow(r, trainer, f)
}

#' ARX classifier argument constructor
#'
#' Constructs a list of arguments for [arx_classifier()].
#'
#' @inheritParams arx_args_list
#' @param outcome_transform Scalar character. Whether the outcome should
#'   be created using growth rates (as the predictors are) or lagged
#'   differences. The second case is closer to the requirements for the
#'   [2022-23 CDC Flusight Hospitalization Experimental Target](https://github.com/cdcepi/Flusight-forecast-data/blob/745511c436923e1dc201dea0f4181f21a8217b52/data-experimental/README.md).
#'   See the Classification Vignette for details of how to create a reasonable
#'   baseline for this case. Selecting `"growth_rate"` (the default) uses
#'   [epiprocess::growth_rate()] to create the outcome using some of the
#'   additional arguments below. Choosing `"lag_difference"` instead simply
#'   uses the change from the value at the selected `horizon`.
#' @param breaks Vector. A vector of breaks to turn real-valued growth rates
#'   into discrete classes. The default gives binary upswing classification
#'   as in [McDonald, Bien, Green, Hu, et al.](https://doi.org/10.1073/pnas.2111453118).
#'   This coincides with the default `trainer = parsnip::logistic_reg()`
#'   argument in [arx_classifier()]. However, multiclass classification is also
#'   supported (e.g. with `breaks = c(-.2, .25)`) provided that
#'   `trainer = parsnip::multinom_reg()` (or another multiclass trainer)
#'   is used as well. These will be sliently expanded to cover the entire
#'   real line (so the default will become `breaks = c(-Inf, .25, Inf)`) before
#'   being used to discretize the response. This is different than the
#'   behaviour in [recipes::step_cut()] which creates classes that only cover
#'   the range of the training data.
#' @param horizon Scalar integer. This is passed to the `h` argument of
#'   [epiprocess::growth_rate()]. It determines the amount of data used to
#'   calculate the growth rate.
#' @param method Character. Options available for growth rate calculation.
#' @param log_scale Scalar logical. Whether to compute growth rates on the
#'   log scale.
#' @param additional_gr_args List. Optional arguments controlling growth rate
#'   calculation. See [epiprocess::growth_rate()] and the related Vignette for
#'   more details.
#'
#' @return A list containing updated parameter choices with class `arx_clist`.
#' @export
#'
#' @examples
#' arx_class_args_list()
#'
#' # 3-class classsification,
#' # also needs arx_classifier(trainer = parsnip::multinom_reg())
#' arx_class_args_list(breaks = c(-.2, .25))
arx_class_args_list <- function(
    lags = c(0L, 7L, 14L),
    ahead = 7L,
    n_training = Inf,
    forecast_date = NULL,
    target_date = NULL,
    outcome_transform = c("growth_rate", "lag_difference"),
    breaks = 0.25,
    horizon = 7L,
    method = c("rel_change", "linear_reg", "smooth_spline", "trend_filter"),
    log_scale = FALSE,
    additional_gr_args = list(),
    nafill_buffer = Inf,
    ...) {
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)
  method <- match.arg(method)
  outcome_transform <- match.arg(outcome_transform)

  arg_is_scalar(ahead, n_training, horizon, log_scale)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_date(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, lags, horizon)
  arg_is_numeric(breaks)
  arg_is_lgl(log_scale)
  arg_is_pos(n_training)
  if (is.finite(n_training)) arg_is_pos_int(n_training)
  if (is.finite(nafill_buffer)) arg_is_pos_int(nafill_buffer, allow_null = TRUE)
  if (!is.list(additional_gr_args)) {
    cli::cli_abort(
      c("`additional_gr_args` must be a {.cls list}.",
        "!" = "This is a {.cls {class(additional_gr_args)}}.",
        i = "See `?epiprocess::growth_rate` for available arguments."
      )
    )
  }

  breaks <- sort(breaks)
  if (min(breaks) > -Inf) breaks <- c(-Inf, breaks)
  if (max(breaks) < Inf) breaks <- c(breaks, Inf)


  max_lags <- max(lags)
  structure(
    enlist(
      lags = .lags,
      ahead,
      n_training,
      breaks,
      forecast_date,
      target_date,
      outcome_transform,
      max_lags,
      horizon,
      method,
      log_scale,
      additional_gr_args,
      nafill_buffer
    ),
    class = c("arx_class", "alist")
  )
}

#' @export
print.arx_class <- function(x, ...) {
  name <- "ARX Classifier"
  NextMethod(name = name, ...)
}

# this is a trivial change to induce a check
