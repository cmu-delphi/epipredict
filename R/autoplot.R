#' @importFrom ggplot2 autoplot aes geom_point geom_line geom_ribbon geom_linerange
#' @export
ggplot2::autoplot

#' Automatically plot an `epi_workflow` or `canned_epipred` object
#'
#' For a fit workflow, the training data will be displayed, the response by
#' default. If `predictions` is not `NULL` then point and interval forecasts
#' will be shown as well. Unfit workflows will result in an error, (you
#' can simply call `autoplot()` on the original `epi_df`).
#'
#'
#'
#'
#' @inheritParams epiprocess::autoplot.epi_df
#' @param object,x An `epi_workflow`
#' @param predictions A data frame with predictions. If `NULL`, only the
#'   original data is shown.
#' @param observed_response An epi_df of the data to plot against. This is for the case
#'   where you have the actual results to compare the forecast against.
#' @param .levels A numeric vector of levels to plot for any prediction bands.
#'   More than 3 levels begins to be difficult to see.
#' @param ... Ignored
#' @param .facet_by Similar to `.color_by` except that the default is to
#'   display the response.
#' @param .base_color If available, prediction bands will be shown with this
#'   color.
#' @param .point_pred_color If available, point forecasts will be shown with
#'   this color.
#'
#' @name autoplot-epipred
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value >= as.Date("2021-11-01"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_naomit()
#'
#' f <- frosting() %>%
#'   layer_residual_quantiles() %>%
#'   layer_threshold(starts_with(".pred")) %>%
#'   layer_add_target_date()
#'
#' wf <- epi_workflow(r, linear_reg(), f) %>% fit(jhu)
#'
#' autoplot(wf)
#'
#' latest <- jhu %>% filter(time_value >= max(time_value) - 14)
#' preds <- predict(wf, latest)
#' autoplot(wf, preds, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt"))
#'
#' # ------- Show multiple horizons
#'
#' p <- lapply(c(7, 14, 21, 28), function(h) {
#'   r <- epi_recipe(jhu) %>%
#'     step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'     step_epi_ahead(death_rate, ahead = h) %>%
#'     step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'     step_epi_naomit()
#'   ewf <- epi_workflow(r, linear_reg(), f) %>% fit(jhu)
#'   forecast(ewf)
#' })
#'
#' p <- do.call(rbind, p)
#' autoplot(wf, p, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt"))
#'
#' # ------- Plotting canned forecaster output
#'
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value >= as.Date("2021-11-01"))
#' flat <- flatline_forecaster(jhu, "death_rate")
#' autoplot(flat, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt"))
#'
#' arx <- arx_forecaster(jhu, "death_rate", c("case_rate", "death_rate"),
#'   args_list = arx_args_list(ahead = 14L)
#' )
#' autoplot(arx, .facet_filter = geo_value %in% c("ca", "ny", "de", "mt", "mo", "in"))
NULL

#' @export
#' @rdname autoplot-epipred
autoplot.epi_workflow <- function(
    object,
    predictions = NULL,
    observed_response = NULL,
    .levels = c(.5, .8, .9), ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "dodgerblue4",
    .point_pred_color = "orange",
    .facet_filter = NULL,
    .max_facets = deprecated()) {
  rlang::check_dots_empty()
  arg_is_probabilities(.levels)
  rlang::arg_match(.color_by)
  rlang::arg_match(.facet_by)

  if (!workflows::is_trained_workflow(object)) {
    cli_abort(c(
      "Can't plot an untrained {.cls epi_workflow}.",
      i = "Do you need to call `fit()`?"
    ))
  }

  mold <- workflows::extract_mold(object)
  y <- mold$outcomes
  if (ncol(y) > 1) {
    y <- y[, 1]
    cli_warn("Multiple outcome variables were detected. Displaying only 1.")
  }
  keys <- c("geo_value", "time_value", "key")
  mold_roles <- names(mold$extras$roles)
  # extract the relevant column names for plotting
  if (starts_with_impl("ahead_", names(y)) || starts_with_impl("lag_", names(y))) {
    old_name_y <- unlist(strsplit(names(y), "_"))
    new_name_y <- paste(old_name_y[-c(1:2)], collapse = "_")
  } else {
    new_name_y <- names(y)
  }
  if (is.null(observed_response)) {
    # the outcome has shifted, so we need to shift it forward (or back)
    # by the corresponding amount
    observed_response <- bind_cols(mold$extras$roles[mold_roles %in% keys], y)
    if (starts_with_impl("ahead_", names(y))) {
      shift <- as.numeric(old_name_y[2])
    } else if (starts_with_impl("lag_", names(y))) {
      old_name_y <- unlist(strsplit(names(y), "_"))
      shift <- -as.numeric(old_name_y[2])
    } else {
      new_name_y <- names(y)
      shift <- 0
    }
    observed_response <- rename(observed_response, !!new_name_y := !!names(y))
    if (!is.null(shift)) {
      observed_response <- mutate(observed_response, time_value = time_value + shift)
    }
    other_keys <- setdiff(key_colnames(object), c("geo_value", "time_value"))
    observed_response <- as_epi_df(observed_response,
      as_of = object$fit$meta$as_of,
      other_keys = other_keys
    )
  }
  if (is.null(predictions)) {
    return(autoplot(
      observed_response, new_name_y,
      .color_by = .color_by, .facet_by = .facet_by, .base_color = .base_color,
      .facet_filter = {{ .facet_filter }},
      .max_facets = .max_facets
    ))
  }

  if ("target_date" %in% names(predictions)) {
    if ("time_value" %in% names(predictions)) {
      predictions <- select(predictions, -time_value)
    }
    predictions <- rename(predictions, time_value = target_date)
  }
  pred_cols_ok <- hardhat::check_column_names(predictions, key_colnames(observed_response))
  if (!pred_cols_ok$ok) {
    cli_warn(c(
      "`predictions` is missing required variables: {.var {pred_cols_ok$missing_names}}.",
      i = "Plotting the original data."
    ))
    return(autoplot(
      observed_response, !!new_name_y,
      .color_by = .color_by, .facet_by = .facet_by, .base_color = .base_color,
      .facet_filter = {{ .facet_filter }},
      .max_facets = .max_facets
    ))
  }

  # First we plot the history, always faceted by everything
  bp <- autoplot(observed_response, !!new_name_y,
    .color_by = "none", .facet_by = "all_keys",
    .base_color = "black", .facet_filter = {{ .facet_filter }},
    .max_facets = .max_facets
  )

  # Now, prepare matching facets in the predictions
  ek <- epi_keys_only(observed_response)
  predictions <- predictions %>%
    mutate(
      .facets = interaction(!!!rlang::syms(as.list(ek)), sep = " / "),
    )
  .facet_filter <- rlang::enquo(.facet_filter)
  if (!rlang::quo_is_null(.facet_filter) && ".facets" %in% names(bp$data)) {
    predictions <- filter(predictions, .facets %in% unique(bp$data$.facets)) %>%
      mutate(.facets = droplevels(.facets))
  }


  if (".pred_distn" %in% names(predictions)) {
    bp <- plot_bands(bp, predictions, .levels, .base_color)
  }

  if (".pred" %in% names(predictions)) {
    ntarget_dates <- dplyr::n_distinct(predictions$time_value)
    if (ntarget_dates > 1L) {
      bp <- bp +
        geom_line(
          data = predictions, aes(y = .data$.pred),
          color = .point_pred_color
        )
    } else {
      bp <- bp +
        geom_point(
          data = predictions, aes(y = .data$.pred),
          color = .point_pred_color
        )
    }
  }
  bp
}

#' @export
#' @rdname autoplot-epipred
autoplot.canned_epipred <- function(
    object, observed_response = NULL, ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "dodgerblue4",
    .point_pred_color = "orange",
    .facet_filter = NULL,
    .max_facets = deprecated()) {
  rlang::check_dots_empty()
  rlang::arg_match(.color_by)
  rlang::arg_match(.facet_by)

  ewf <- object$epi_workflow
  predictions <- object$predictions %>%
    rename(time_value = target_date)

  autoplot(ewf, predictions, observed_response, ...,
    .color_by = .color_by, .facet_by = .facet_by,
    .base_color = .base_color, .facet_filter = {{ .facet_filter }},
    .max_facets = .max_facets
  )
}

#' @export
#' @rdname autoplot-epipred
plot.epi_workflow <- function(x, ...) {
  autoplot(x, ...)
}

#' @export
#' @rdname autoplot-epipred
plot.canned_epipred <- function(x, ...) {
  autoplot(x, ...)
}


starts_with_impl <- function(x, vars) {
  n <- nchar(x)
  x == substr(vars, 1, n)
}

plot_bands <- function(
    base_plot, predictions,
    levels = c(.5, .8, .9),
    fill = "blue4",
    alpha = 0.6,
    linewidth = 0.05) {
  innames <- names(predictions)
  n_levels <- length(levels)
  alpha <- alpha / (n_levels - 1)
  # generate the corresponding level that is 1 - level
  levels <- (1 - levels) / 2
  levels <- c(rev(levels), 1 - levels)

  ntarget_dates <- dplyr::n_distinct(predictions$time_value)

  predictions <- predictions %>%
    mutate(.pred_distn = quantile_pred(quantile(.pred_distn, levels), levels)) %>%
    pivot_quantiles_wider(.pred_distn)
  qnames <- setdiff(names(predictions), innames)

  for (i in 1:n_levels) {
    bottom <- qnames[i]
    top <- rev(qnames)[i]
    if (i == 1) {
      if (ntarget_dates > 1L) {
        base_plot <- base_plot +
          geom_ribbon(
            data = predictions,
            aes(ymin = .data[[bottom]], ymax = .data[[top]]),
            alpha = 0.2, linewidth = linewidth, fill = fill
          )
      } else {
        base_plot <- base_plot +
          geom_linerange(
            data = predictions,
            aes(ymin = .data[[bottom]], ymax = .data[[top]]),
            alpha = 0.2, linewidth = 2, color = fill
          )
      }
    } else {
      if (ntarget_dates > 1L) {
        base_plot <- base_plot +
          geom_ribbon(
            data = predictions,
            aes(ymin = .data[[bottom]], ymax = .data[[top]]),
            fill = fill, alpha = alpha
          )
      } else {
        base_plot <- base_plot +
          geom_linerange(
            data = predictions,
            aes(ymin = .data[[bottom]], ymax = .data[[top]]),
            color = fill, alpha = alpha, linewidth = 2
          )
      }
    }
  }
  base_plot
}

find_level <- function(x) {
  unique((x < .5) * (1 - 2 * x) + (x > .5) * (1 - 2 * (1 - x)))
}
