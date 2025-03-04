#' Direct autoregressive classifier with covariates
#'
#'
#' @description
#' This is an autoregressive classification model for continuous data. It does
#'   "direct" forecasting, meaning that it estimates a class at a particular
#'   target horizon.
#'
#' @details
#' The `arx_classifier()` is an autoregressive classification model for `epi_df`
#'   data that is used to predict a discrete class for each case under
#'   consideration.  It is a direct forecaster in that it estimates the classes
#'   at a specific horizon or ahead value.
#'
#' To get a sense of how the `arx_classifier()` works, let's consider a simple
#'   example with minimal inputs. For this, we will use the built-in
#'   `covid_case_death_rates` that contains confirmed COVID-19 cases and deaths
#'   from JHU CSSE for all states over Dec 31, 2020 to Dec 31, 2021. From this,
#'   we'll take a subset of data for five states over June 4, 2021 to December
#'   31, 2021. Our objective is to predict whether the case rates are increasing
#'   when considering the 0, 7 and 14 day case rates:
#'
#' ```{r}
#' jhu <- covid_case_death_rates %>%
#'   filter(
#'     time_value >= "2021-06-04",
#'     time_value <= "2021-12-31",
#'     geo_value %in% c("ca", "fl", "tx", "ny", "nj")
#'   )
#'
#' out <- arx_classifier(jhu, outcome = "case_rate", predictors = "case_rate")
#'
#' out$predictions
#' ```
#'
#' The key takeaway from the predictions is that there are two prediction
#'   classes: (-Inf, 0.25] and (0.25, Inf). This is because for our goal of
#'   classification the classes must be discrete. The discretization of the
#'   real-valued outcome is controlled by the `breaks` argument, which defaults
#'   to 0.25. Such breaks will be automatically extended to cover the entire
#'   real line. For example, the default break of 0.25 is silently extended to
#'   breaks = c(-Inf, .25, Inf) and, therefore, results in two classes: [-Inf,
#'   0.25] and (0.25, Inf). These two classes are used to discretize the
#'   outcome. The conversion of the outcome to such classes is handled
#'   internally. So if discrete classes already exist for the outcome in the
#'   `epi_df`, then we recommend to code a classifier from scratch using the
#'   `epi_workflow` framework for more control.
#'
#' The `trainer` is a `parsnip` model describing the type of estimation such
#'   that `mode = "classification"` is enforced. The two typical trainers that
#'   are used are `parsnip::logistic_reg()` for two classes or
#'   `parsnip::multinom_reg()` for more than two classes.
#'
#' ```{r}
#' workflows::extract_spec_parsnip(out$epi_workflow)
#' ```
#'
#' From the parsnip model specification, we can see that the trainer used is
#'   logistic regression, which is expected for our binary outcome. More
#'   complicated trainers like `parsnip::naive_Bayes()` or
#'   `parsnip::rand_forest()` may also be used (however, we will stick to the
#'   basics in this gentle introduction to the classifier).
#'
#' If you use the default trainer of logistic regression for binary
#'   classification and you decide against using the default break of 0.25, then
#'   you should only input one break so that there are two classification bins
#'   to properly dichotomize the outcome. For example, let's set a break of 0.5
#'   instead of relying on the default of 0.25. We can do this by passing 0.5 to
#'   the `breaks` argument in `arx_class_args_list()` as follows:
#'
#' ```{r}
#' out_break_0.5 <- arx_classifier(
#'   jhu,
#'   outcome = "case_rate",
#'   predictors = "case_rate",
#'   args_list = arx_class_args_list(
#'     breaks = 0.5
#'   )
#' )
#'
#' out_break_0.5$predictions
#' ```
#' Indeed, we can observe that the two `.pred_class` are now (-Inf, 0.5] and
#'   (0.5, Inf). See `help(arx_class_args_list)` for other available
#'   modifications.
#'
#' Additional arguments that may be supplied to `arx_class_args_list()` include
#'   the expected `lags` and `ahead` arguments for an autoregressive-type model.
#'   These have default values of 0, 7, and 14 days for the lags of the
#'   predictors and 7 days ahead of the forecast date for predicting the
#'   outcome. There is also `n_training` to indicate the upper bound for the
#'   number of training rows per key. If you would like some practice with using
#'   this, then remove the filtering command to obtain data within "2021-06-04"
#'   and "2021-12-31" and instead set `n_training` to be the number of days
#'   between these two dates, inclusive of the end points. The end results
#'   should be the same. In addition to `n_training`, there are `forecast_date`
#'   and `target_date` to specify the date that the forecast is created and
#'   intended, respectively. We will not dwell on such arguments here as they
#'   are not unique to this classifier or absolutely essential to understanding
#'   how it operates. The remaining arguments will be discussed organically, as
#'   they are needed to serve our purposes. For information on any remaining
#'   arguments that are not discussed here, please see the function
#'   documentation for a complete list and their definitions.
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
#' tiny_geos <- c("as", "mp", "vi", "gu", "pr")
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value >= as.Date("2021-11-01"), !(geo_value %in% tiny_geos))
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
    trainer = logistic_reg(),
    args_list = arx_class_args_list()) {
  if (!is_classification(trainer)) {
    cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'classification'.")
  }

  wf <- arx_class_epi_workflow(epi_data, outcome, predictors, trainer, args_list)
  wf <- fit(wf, epi_data)

  if (args_list$adjust_latency == "none") {
    forecast_date_default <- max(epi_data$time_value)
    if (!is.null(args_list$forecast_date) && args_list$forecast_date != forecast_date_default) {
      cli_warn(
        "The specified forecast date {args_list$forecast_date} doesn't match the
        date from which the forecast is occurring {forecast_date}."
      )
    }
  } else {
    forecast_date_default <- attributes(epi_data)$metadata$as_of
  }
  forecast_date <- args_list$forecast_date %||% forecast_date_default
  target_date <- args_list$target_date %||% (forecast_date + args_list$ahead)
  preds <- forecast(
    wf,
  ) %>%
    as_tibble() %>%
    select(-time_value)

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
#' @param trainer A `{parsnip}` model describing the type of estimation. For
#'  now, we enforce `mode = "classification"`. Typical values are
#'  [parsnip::logistic_reg()] or [parsnip::multinom_reg()]. More complicated
#'  trainers like [parsnip::naive_Bayes()] or [parsnip::rand_forest()] can also
#'  be used. May be `NULL` if you'd like to decide later.
#'
#' @return An unfit `epi_workflow`.
#' @export
#' @seealso [arx_classifier()] [arx_class_args_list()]
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value >= as.Date("2021-11-01"))
#'
#' arx_class_epi_workflow(jhu, "death_rate", c("case_rate", "death_rate"))
#'
#' arx_class_epi_workflow(
#'   jhu,
#'   "death_rate",
#'   c("case_rate", "death_rate"),
#'   trainer = multinom_reg(),
#'   args_list = arx_class_args_list(
#'     breaks = c(-.05, .1), ahead = 14,
#'     horizon = 14, method = "linear_reg"
#'   )
#' )
arx_class_epi_workflow <- function(
    epi_data,
    outcome,
    predictors,
    trainer = parsnip::logistic_reg(),
    args_list = arx_class_args_list()) {
  validate_forecaster_inputs(epi_data, outcome, predictors)
  if (!inherits(args_list, c("arx_class", "alist"))) {
    cli_abort("`args_list` was not created using `arx_class_args_list()`.")
  }
  if (!(is.null(trainer) || is_classification(trainer))) {
    cli_abort("`trainer` must be a {.pkg parsnip} model of mode 'classification'.")
  }

  if (args_list$adjust_latency == "none") {
    forecast_date_default <- max(epi_data$time_value)
    if (!is.null(args_list$forecast_date) && args_list$forecast_date != forecast_date_default) {
      cli_warn("The specified forecast date {args_list$forecast_date} doesn't match the date from which the forecast is occurring {forecast_date}.")
    }
  } else {
    forecast_date_default <- attributes(epi_data)$metadata$as_of
  }
  forecast_date <- args_list$forecast_date %||% forecast_date_default
  target_date <- args_list$target_date %||% (forecast_date + args_list$ahead)

  lags <- arx_lags_validator(predictors, args_list$lags)

  # --- preprocessor
  # ------- predictors
  r <- epi_recipe(epi_data) %>%
    step_growth_rate(
      all_of(predictors),
      role = "grp",
      horizon = args_list$horizon,
      method = args_list$method,
      log_scale = args_list$log_scale
    )
  for (l in seq_along(lags)) {
    pred_names <- predictors[l]
    pred_names <- as.character(glue::glue_data(
      args_list, "gr_{horizon}_{method}_{pred_names}"
    ))
    r <- step_epi_lag(r, !!pred_names, lag = lags[[l]])
  }
  # ------- outcome
  if (args_list$outcome_transform == "lag_difference") {
    pre_out_name <- as.character(
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
    pre_out_name <- as.character(
      glue::glue_data(args_list, "gr_{horizon}_{method}_{outcome}")
    )
    if (!(outcome %in% predictors)) {
      r <- r %>%
        step_growth_rate(
          !!outcome,
          role = "pre-outcome",
          horizon = args_list$horizon,
          method = args_list$method,
          log_scale = args_list$log_scale
        )
    }
  }
  # regex that will match any amount of adjustment for the ahead
  ahead_out_name_regex <- glue::glue("ahead_[0-9]*_{pre_out_name}")
  method_adjust_latency <- args_list$adjust_latency
  if (method_adjust_latency != "none") {
    if (method_adjust_latency != "extend_ahead") {
      cli_abort("only extend_ahead is currently supported",
        class = "epipredict__arx_classifier__adjust_latency_unsupported_method"
      )
    }
    r <- r %>% step_adjust_latency(!!pre_out_name,
      fixed_forecast_date = forecast_date,
      method = method_adjust_latency
    )
  }
  r <- r %>%
    step_epi_ahead(!!pre_out_name, ahead = args_list$ahead, role = "pre-outcome")
  r <- r %>%
    step_mutate(
      across(
        matches(ahead_out_name_regex),
        ~ cut(.x, breaks = args_list$breaks),
        .names = "outcome_class",
        .unpack = TRUE
      ),
      role = "outcome"
    ) %>%
    step_epi_naomit() %>%
    step_training_window(n_recent = args_list$n_training)

  if (!is.null(args_list$check_enough_data_n)) {
    r <- check_enough_data(
      r,
      recipes::all_predictors(),
      recipes::all_outcomes(),
      n = args_list$check_enough_data_n,
      epi_keys = args_list$check_enough_data_epi_keys,
      drop_na = FALSE
    )
  }

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
#' @param check_enough_data_n Integer. A lower limit for the number of rows per
#'   epi_key that are required for training. If `NULL`, this check is ignored.
#' @param check_enough_data_epi_keys Character vector. A character vector of
#'   column names on which to group the data and check threshold within each
#'   group. Useful if training per group (for example, per geo_value).
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
    adjust_latency = c("none", "extend_ahead", "extend_lags", "locf"),
    warn_latency = TRUE,
    outcome_transform = c("growth_rate", "lag_difference"),
    breaks = 0.25,
    horizon = 7L,
    method = c("rel_change", "linear_reg"),
    log_scale = FALSE,
    check_enough_data_n = NULL,
    check_enough_data_epi_keys = NULL,
    ...) {
  rlang::check_dots_empty()
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)
  method <- rlang::arg_match(method)
  outcome_transform <- rlang::arg_match(outcome_transform)

  adjust_latency <- rlang::arg_match(adjust_latency)
  arg_is_scalar(ahead, n_training, horizon, log_scale, adjust_latency, warn_latency)
  arg_is_scalar(forecast_date, target_date, allow_null = TRUE)
  arg_is_date(forecast_date, target_date, allow_null = TRUE)
  arg_is_nonneg_int(ahead, lags, horizon)
  arg_is_numeric(breaks)
  arg_is_lgl(log_scale)
  arg_is_pos(n_training)
  if (is.finite(n_training)) arg_is_pos_int(n_training)
  arg_is_pos(check_enough_data_n, allow_null = TRUE)
  arg_is_chr(check_enough_data_epi_keys, allow_null = TRUE)

  if (!is.null(forecast_date) && !is.null(target_date)) {
    if (forecast_date + ahead != target_date) {
      cli_warn(
        "`forecast_date` {.val {forecast_date}} +
        `ahead` {.val {ahead}} must equal `target_date` {.val {target_date}}.",
        class = "epipredict__arx_args__inconsistent_target_ahead_forecaste_date"
      )
    }
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
      adjust_latency,
      outcome_transform,
      max_lags,
      horizon,
      method,
      log_scale,
      check_enough_data_n,
      check_enough_data_epi_keys
    ),
    class = c("arx_class", "alist")
  )
}

#' @export
print.arx_class <- function(x, ...) {
  name <- "ARX Classifier"
  NextMethod(name = name, ...)
}
