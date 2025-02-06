#' Calculate a climatological variable based on the history
#'
#' `step_climate()` creates a *specification* of a recipe step
#'   that will generate one or more new columns of derived data. Using this as
#'   a predictor is most useful when predicting strongly seasonal data. The
#'   climate timing will automatically be aligned to the outcome at bake time.
#'
#'
#' @inheritParams step_growth_rate
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
#' @template step-return
#'
#'
#' @export
#' @examples
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_climate(death_rate, time_type = "day")
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
           time_type = c("epiweek", "week", "month", "day"),
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
            i = "Here, the data is in {.val {edf_time_type}} while
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
                        epiweek = lubridate::epiweek, week = lubridate::week,
                        month = lubridate::month, day = lubridate::yday)

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

  modulus <- switch(x$time_type, epiweek = 53L, week = 53L, month = 12L, day = 365L)

  climate_table <- training %>%
    mutate(.idx = x$time_aggr(time_value), .weights = wts) %>%
    select(.idx, .weights, c(col_names, x$epi_keys)) %>%
    tidyr::pivot_longer(all_of(unname(col_names))) %>%
    dplyr::reframe(
      roll_modular_multivec(value, .idx, .weights, x$center_method,
                              x$window_size, modulus),
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
  climate_table <- object$climate_table %>%
    mutate(
      .idx = (.idx - object$forecast_ahead) %% object$modulus,
      .idx = dplyr::case_when(.idx == 0 ~ object$modulus, TRUE ~ .idx)
    )
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

roll_modular_multivec <- function(col, .idx, weights, center_method,
                                  window_size, modulus) {
  if (center_method == "mean") {
    aggr <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  } else {
    aggr <- function(x, w) median(x, na.rm = TRUE)
  }
  tib <- tibble(col = col, weights = weights, .idx = .idx) |>
    arrange(.idx) |>
    tidyr::nest(data = c(col, weights), .by = .idx)
  out <- double(nrow(tib))
  for (iter in seq_along(out)) {
    entries <- (iter - window_size):(iter + window_size) %% modulus
    entries[entries == 0] <- modulus
    out[iter] <- with(
      purrr::list_rbind(tib$data[entries]),
      aggr(col, weights)
    )
  }
  tibble(.idx = unique(tib$.idx), climate_pred = out)
}
