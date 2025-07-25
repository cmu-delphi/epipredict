#' Calculate a climatological variable based on the history
#'
#' `step_climate()` creates a *specification* of a recipe step that will
#' generate one or more new columns of derived data. This step examines all
#' available seasons in the training data and calculates the a measure of center
#' for the "typical" season. Think of this like with the weather: to predict the
#' temperature in January in Pittsburgh, PA, I might look at all previous
#' January's on record, average their temperatures, and include that in my
#' model. So it is important to _align_ the forecast horizon with the climate.
#' This step will work best if added after `step_epi_ahead()`, but that is not
#' strictly required. See the details for more information.
#'
#' @details
#' Construction of a climate predictor can be helpful with strongly seasonal
#' data. But its utility is greatest when the estimated "climate" is aligned
#' to the forecast horizon.
#' For example, if today is December 1, and we want
#' to make a prediction for December 15, we want to know the climate for the
#' week of December 15 to use in our model. But we also want to align the rest
#' of our training data with the climate _2 weeks after_ those dates.
#'
#' To accomplish
#' this, if we have daily data, we could use `time_type = "week"` and
#' `forecast_ahead = 2`. The climate predictor would be created by taking
#' averages over each week (with a window of a few weeks before and after, as
#' determined by `window_size`), and then aligning these with the appropriate dates
#' in the training data so that each `time_value` will "see" the typical climate 2
#' weeks in the future.
#'
#' Alternatively, in the same scenario, we could use `time_type = "day"` and
#' `forecast_ahead = 14`. The climate predictor would be created by taking
#' averages over a small window around each _day_, and then aligning these with
#' the appropriate dates in the training data so that each `time_value` will
#' "see" the climate 14 days in the future.
#'
#' The only differences between these options is the type of averaging performed
#' over the historical data. In the first case, days in the same week will get
#' the same value of the climate predictor (because we're looking at weekly
#' windows), while in the second case, every day in the data will have the
#' average climate for the _day_ that happens 14 days in the future.
#'
#' Autodetecting the forecast horizon can only be guaranteed to work correctly
#' when the time types are the same: for example using daily data for training
#' and daily climate calculations. However, using weekly data, predicting 4
#' weeks ahead, and setting `time_type = "month"` is perfectly reasonable. It's
#' just that the climate is calculated over _months_ (January, February, March,
#' etc.) so how to properly align this when producing a forecast for the 5th week
#' in the year is challenging. For scenarios like these, it may be best to
#' approximately match the times with `forecast_ahead = 1`, for example.
#'
#'
#' @inheritParams step_growth_rate
#' @param forecast_ahead The forecast horizon. By default, this step will try to
#'   detect whether a forecast horizon has already been specified with
#'   [step_epi_ahead()]. Alternatively, one can specify an explicit
#'   horizon with a scalar integer. Auto-detection is only possible
#'   when the time type of the `epi_df` used to create the `epi_recipe` is the
#'   same as the aggregation
#'   `time_type` specified in this step (say, both daily or both weekly). If,
#'   for example, daily data is used with monthly time aggregation, then
#'   auto-detection is not possible (and may in fact lead to strange behaviour
#'   even if `forecast_ahead` is specified with an integer). See details below.
#' @param time_type The duration over which time aggregation should be performed.
#' @param center_method The measure of center to be calculated over the time
#'   window.
#' @param window_size Scalar integer. How many time units on each side should
#'   be included. For example, if `window_size = 3` and `time_type = "day"`,
#'   then on each day in the data, the center will be calculated using 3 days
#'   before and three days after. So, in this case, it operates like a weekly
#'   rolling average, centered at each day.
#' @param epi_keys Character vector or `NULL`. Any columns mentioned will be
#'   grouped before performing any center calculation. So for example, given
#'   state-level data, a national climate would be calculated if `NULL`, but
#'   passing `epi_keys = "geo_value"` would calculate the climate separately
#'   by state.
#' @param role What role should be assigned for any variables created by this
#'   step? "predictor" is the most likely choice.
#' @template step-return
#'
#'
#' @export
#' @examples
#' # automatically detects the horizon
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_climate(death_rate, time_type = "day")
#' r
#'
#' r %>%
#'   prep(covid_case_death_rates) %>%
#'   bake(new_data = NULL)
#'
#' # same idea, but using weekly climate
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_climate(death_rate,
#'     forecast_ahead = 1, time_type = "epiweek",
#'     window_size = 1L
#'   )
#' r
#'
#' r %>%
#'   prep(covid_case_death_rates) %>%
#'   bake(new_data = NULL)
#'
#' # switching the order is possible if you specify `forecast_ahead`
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_climate(death_rate, forecast_ahead = 7, time_type = "day") %>%
#'   step_epi_ahead(death_rate, ahead = 7)
#' r
#'
#' r %>%
#'   prep(covid_case_death_rates) %>%
#'   bake(new_data = NULL)
step_climate <-
  function(recipe,
           ...,
           forecast_ahead = "detect",
           role = "predictor",
           time_type = c("detect", "epiweek", "week", "month", "day"),
           center_method = c("median", "mean"),
           window_size = 3L,
           epi_keys = NULL,
           prefix = "climate_",
           skip = FALSE,
           id = rand_id("climate")) {
    if (!is_epi_recipe(recipe)) {
      cli_abort("This recipe step can only operate on an {.cls epi_recipe}.")
    }

    ## Handle ahead autodetection, single outcome, time type
    n_outcomes <- sum(recipe$var_info$role == "outcome")
    time_type <- rlang::arg_match(time_type)
    edf_time_type <- attr(recipe$template, "metadata")$time_type
    if (time_type == "detect") time_type <- edf_time_type
    if (edf_time_type == "custom") {
      cli_abort("This step only works with daily, weekly, or yearmonth data.")
    }
    if (n_outcomes > 1L) {
      cli_abort("Only one {.var outcome} role can be used with this step.")
    }
    if (is.character(forecast_ahead)) {
      forecast_ahead <- rlang::arg_match(forecast_ahead)
      if (detect_step(recipe, "epi_ahead")) {
        outcomes <- extract_argument(recipe, "step_epi_ahead", "role") == "outcome"
        forecast_ahead <- extract_argument(recipe, "step_epi_ahead", "ahead")[outcomes]
        if (length(forecast_ahead) != 1L) {
          cli_abort(c(
            "To detect the `forecast_ahead` automatically, `step_epi_ahead()`
            with role = 'outcome' must be specified.",
            i = "Check your recipe, or specify this argument directly in `step_climate()`."
          ))
        }
        ttype_ord <- match(time_type, c("day", "epiweek", "week", "month"))
        ttype_ord <- ttype_ord - as.integer(ttype_ord > 2)
        edf_ttype_ord <- match(edf_time_type, c("day", "week", "yearmonth"))
        if (ttype_ord != edf_ttype_ord) {
          cli_abort(c("Automatic detection of the `forecast_ahead` is only
            supported if the original data and the time type for aggregation
            are in the same units.",
            i = "Here, the data is in {.val {edf_time_type}}s while
            `time_type` is {.val {time_type}}.",
            i = "This is resolved most easily by specifying `forecast_ahead`."
          ))
        }
      } else {
        forecast_ahead <- 0L
      }
    }
    arg_is_int(forecast_ahead)

    # check other args
    center_method <- rlang::arg_match(center_method)
    arg_is_chr(role)
    arg_is_chr(epi_keys, allow_null = TRUE)
    arg_is_nonneg_int(window_size)
    arg_is_scalar(window_size)
    arg_is_chr_scalar(prefix, id)
    arg_is_lgl_scalar(skip)

    time_aggr <- switch(time_type,
      epiweek = epiweek_leap,
      week = isoweek_leap,
      month = lubridate::month,
      day = yday_leap
    )

    recipes::add_step(
      recipe,
      step_climate_new(
        terms = enquos(...),
        role = role,
        trained = FALSE,
        forecast_ahead = forecast_ahead,
        time_type = time_type,
        time_aggr = time_aggr,
        modulus = NULL,
        center_method = center_method,
        window_size = window_size,
        epi_keys = epi_keys,
        climate_table = NULL,
        prefix = prefix,
        columns = NULL,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }


step_climate_new <-
  function(terms,
           role,
           trained,
           forecast_ahead,
           time_type,
           time_aggr,
           modulus,
           center_method,
           window_size,
           epi_keys,
           climate_table,
           prefix,
           columns,
           skip,
           id,
           case_weights) {
    recipes::step(
      subclass = "climate",
      terms = terms,
      role = role,
      trained = trained,
      forecast_ahead = forecast_ahead,
      time_type = time_type,
      time_aggr = time_aggr,
      modulus = modulus,
      center_method = center_method,
      window_size = window_size,
      epi_keys = epi_keys,
      climate_table = climate_table,
      prefix = prefix,
      columns = columns,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }



#' @export
prep.step_climate <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))
  wts <- recipes::get_case_weights(info, training)
  wts_used <- !is.null(wts)
  wts <- wts %||% rep(1, nrow(training))

  modulus <- switch(x$time_type,
    epiweek = 52L, # only sometimes true
    week = 52L,
    month = 12L,
    day = 365L # only sometimes true
  )

  fn <- switch(x$center_method,
    mean = function(x, w) stats::weighted.mean(x, w, na.rm = TRUE),
    median = function(x, w) stats::median(x, na.rm = TRUE)
  )
  # suppose it's week 52, and there is no week 53 this year; then
  # as originally written for 1 week ahead this grabs from week 52+1 %% 53
  # which will be week 53, not week 1.
  ahead_period <- switch(x$time_type,
    epiweek = lubridate::weeks(x$forecast_ahead),
    week = lubridate::weeks(x$forecast_ahead),
    month = months(x$forecast_ahead),
    day = lubridate::days(x$forecast_ahead),
  )
  climate_table <-
    training %>%
    mutate(
      # subtracts a month w/o rollover (usual behavior on weeks/days)
      .idx = time_value %m-% ahead_period,
      .idx = x$time_aggr(.idx),
      .weights = wts
    ) %>%
    select(.idx, .weights, all_of(c(col_names, x$epi_keys))) %>%
    tidyr::pivot_longer(all_of(unname(col_names))) %>%
    dplyr::reframe(
      roll_modular_multivec(value, .idx, .weights, fn, x$window_size, modulus),
      .by = c("name", x$epi_keys)
    ) %>%
    tidyr::pivot_wider(
      names_from = "name", values_from = "climate_pred", names_prefix = x$prefix
    )

  step_climate_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    forecast_ahead = x$forecast_ahead,
    time_type = x$time_type,
    time_aggr = x$time_aggr,
    modulus = modulus,
    center_method = x$center_method,
    window_size = x$window_size,
    epi_keys = x$epi_keys,
    climate_table = climate_table,
    prefix = x$prefix,
    columns = col_names,
    skip = x$skip,
    id = x$id,
    case_weights = wts_used
  )
}


#' @export
bake.step_climate <- function(object, new_data, ...) {
  climate_table <- object$climate_table
  new_data %>%
    mutate(.idx = object$time_aggr(time_value)) %>%
    left_join(climate_table, by = c(".idx", object$epi_keys)) %>%
    select(-.idx)
}


#' @export
print.step_climate <- function(x, width = max(20, options()$width - 30), ...) {
  print_epi_step(
    x$columns, x$terms, x$trained,
    title = "Calculating climate_predictor for ",
    conjunction = "by",
    extra_text = paste(x$time_type, "using the", x$center_method)
  )
  invisible(x)
}

#' group col by .idx values and sum windows around each .idx value
#' @param idx_in the relevant periodic part of time value, e.g. the week number,
#'   limited to the relevant range
#' @param col the list of values indexed by `idx_in`
#' @param weights how much to weigh each particular datapoint (also indexed by
#'   `idx_in`)
#' @param aggr the aggregation function, probably Quantile, mean, or median
#' @param window_size the number of .idx entries before and after to include in
#'   the aggregation
#' @param modulus the number of days/weeks/months in the year, not including any
#'   leap days/weeks
#' @importFrom lubridate %m-%
#' @keywords internal
roll_modular_multivec <- function(col, idx_in, weights, aggr, window_size, modulus) {
  # make a tibble where data gives the list of all datapoints with the
  # corresponding .idx
  tib <- tibble(col = col, weights = weights, .idx = idx_in) |>
    arrange(.idx) |>
    tidyr::nest(data = c(col, weights), .by = .idx)
  # storage for the results, includes all possible time indexes
  out <- tibble(.idx = c(1:modulus, 999), climate_pred = double(modulus + 1))
  for (tib_idx in tib$.idx) {
    entries <- within_window(tib_idx, window_size, modulus)
    out$climate_pred[out$.idx == tib_idx] <- with(
      purrr::list_rbind(tib %>% filter(.idx %in% entries) %>% pull(data)),
      aggr(col, weights)
    )
  }
  # filter to only the ones we actually computed
  out %>% filter(.idx %in% idx_in)
}

#' generate the idx values within `window_size` of `target_idx` given that our
#' time value is of the type matching modulus
#' @param target_idx the time index which we're drawing the window around
#' @param window_size the size of the window on one side of `target_idx`
#' @param modulus the number of days/weeks/months in the year, not including any leap days/weeks
#' @keywords internal
within_window <- function(target_idx, window_size, modulus) {
  entries <- (target_idx - window_size):(target_idx + window_size) %% modulus
  entries[entries == 0] <- modulus
  # note that because we are 1-indexing, we're looking for indices that are 1
  # larger than the actual day/week in the year
  if (modulus == 365) {
    # we need to grab just the window around the leap day on the leap day
    if (target_idx == 999) {
      # there's an extra data point in front of the leap day
      entries <- (59 - window_size):(59 + window_size - 1) %% modulus
      entries[entries == 0] <- modulus
      # adding in the leap day itself
      entries <- c(entries, 999)
    } else if ((59 %in% entries) || (60 %in% entries)) {
      # if we're on the Feb/March boundary for daily data, we need to add in the
      # leap day data
      entries <- c(entries, 999)
    }
  } else if (modulus == 52) {
    # we need to grab just the window around the leap week on the leap week
    if (target_idx == 999) {
      entries <- (53 - window_size):(53 + window_size - 1) %% 52
      entries[entries == 0] <- 52
      entries <- c(entries, 999)
    } else if ((52 %in% entries) || (1 %in% entries)) {
      # if we're on the year boundary for weekly data, we need to add in the
      # leap week data (which is the extra week at the end)
      entries <- c(entries, 999)
    }
  }
  entries
}


#' a function that assigns Feb 29th to 999, and aligns all other dates the same
#' number in the year, regardless of whether it's a leap year
#' @keywords internal
#' @importFrom lubridate yday month leap_year
yday_leap <- function(time_value) {
  dplyr::case_when(
    !leap_year(time_value) ~ yday(time_value),
    leap_day(time_value) ~ 999,
    TRUE ~ yday(time_value) - as.numeric(month(time_value) > 2L)
  )
}
leap_day <- function(x) lubridate::month(x) == 2 & lubridate::day(x) == 29
#' epiweek, but it assigns week 53 the value of 999 instead so it mirrors the assignments in yday_leap
#' @keywords internal
epiweek_leap <- function(time_value) {
  week_values <- lubridate::epiweek(time_value)
  week_values[week_values == 53] <- 999
  week_values
}
#' isoweek, but it assigns week 53 the value of 999 instead so it mirrors the assignments in yday_leap
#' @keywords internal
isoweek_leap <- function(time_value) {
  week_values <- lubridate::isoweek(time_value)
  week_values[week_values == 53] <- 999
  week_values
}
