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
#' @param object An `epi_workflow`
#' @param predictions A data frame with predictions. If `NULL`, only the
#'   original data is shown.
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
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value >= as.Date("2021-11-01"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_lag(case_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_naomit()
#'
#' f <- frosting() %>%
#'   layer_residual_quantiles(
#'     quantile_levels = c(.025, .1, .25, .75, .9, .975)
#'   ) %>%
#'   layer_threshold(starts_with(".pred")) %>%
#'   layer_add_target_date()
#'
#' wf <- epi_workflow(r, linear_reg(), f) %>% fit(jhu)
#'
#' autoplot(wf)
#'
#' latest <- jhu %>% filter(time_value >= max(time_value) - 14)
#' preds <- predict(wf, latest)
#' autoplot(wf, preds, .max_facets = 4)
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
#' autoplot(wf, p, .max_facets = 4)
#'
#' # ------- Plotting canned forecaster output
#'
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value >= as.Date("2021-11-01"))
#' flat <- flatline_forecaster(jhu, "death_rate")
#' autoplot(flat, .max_facets = 4)
#'
#' arx <- arx_forecaster(jhu, "death_rate", c("case_rate", "death_rate"),
#'   args_list = arx_args_list(ahead = 14L)
#' )
#' autoplot(arx, .max_facets = 6)
NULL

#' @export
#' @rdname autoplot-epipred
autoplot.epi_workflow <- function(
    object, predictions = NULL,
    .levels = c(.5, .8, .95), ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "dodgerblue4",
    .point_pred_color = "orange",
    .max_facets = Inf) {
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
  edf <- bind_cols(mold$extras$roles[mold_roles %in% keys], y)
  if (starts_with_impl("ahead_", names(y))) {
    old_name_y <- unlist(strsplit(names(y), "_"))
    shift <- as.numeric(old_name_y[2])
    new_name_y <- paste(old_name_y[-c(1:2)], collapse = "_")
    edf <- rename(edf, !!new_name_y := !!names(y))
  } else if (starts_with_impl("lag_", names(y))) {
    old_name_y <- unlist(strsplit(names(y), "_"))
    shift <- -as.numeric(old_name_y[2])
    new_name_y <- paste(old_name_y[-c(1:2)], collapse = "_")
    edf <- rename(edf, !!new_name_y := !!names(y))
  }

  if (!is.null(shift)) {
    edf <- mutate(edf, time_value = time_value + shift)
  }
  extra_keys <- setdiff(key_colnames(object), c("geo_value", "time_value"))
  if (length(extra_keys) == 0L) extra_keys <- NULL
  edf <- as_epi_df(edf,
    as_of = object$fit$meta$as_of,
    additional_metadata = list(other_keys = extra_keys)
  )
  if (is.null(predictions)) {
    return(autoplot(
      edf, new_name_y,
      .color_by = .color_by, .facet_by = .facet_by, .base_color = .base_color,
      .max_facets = .max_facets
    ))
  }

  if ("target_date" %in% names(predictions)) {
    if ("time_value" %in% names(predictions)) {
      predictions <- select(predictions, -time_value)
    }
    predictions <- rename(predictions, time_value = target_date)
  }
  pred_cols_ok <- hardhat::check_column_names(predictions, key_colnames(edf))
  if (!pred_cols_ok$ok) {
    cli_warn(c(
      "`predictions` is missing required variables: {.var {pred_cols_ok$missing_names}}.",
      i = "Plotting the original data."
    ))
    return(autoplot(
      edf, !!new_name_y,
      .color_by = .color_by, .facet_by = .facet_by, .base_color = .base_color,
      .max_facets = .max_facets
    ))
  }

  # First we plot the history, always faceted by everything
  bp <- autoplot(edf, !!new_name_y,
    .color_by = "none", .facet_by = "all_keys",
    .base_color = "black", .max_facets = .max_facets
  )

  # Now, prepare matching facets in the predictions
  ek <- epi_keys_only(edf)
  predictions <- predictions %>%
    mutate(
      .facets = interaction(!!!rlang::syms(as.list(ek)), sep = "/"),
    )
  if (.max_facets < Inf) {
    top_n <- levels(as.factor(bp$data$.facets))[seq_len(.max_facets)]
    predictions <- filter(predictions, .facets %in% top_n) %>%
      mutate(.facets = droplevels(.facets))
  }


  if (".pred_distn" %in% names(predictions)) {
    bp <- plot_bands(bp, predictions, .levels, .base_color)
  }

  if (".pred" %in% names(predictions)) {
    ntarget_dates <- n_distinct(predictions$time_value)
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
    object, ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "dodgerblue4",
    .point_pred_color = "orange",
    .max_facets = Inf) {
  rlang::check_dots_empty()
  rlang::arg_match(.color_by)
  rlang::arg_match(.facet_by)

  ewf <- object$epi_workflow
  predictions <- object$predictions %>%
    dplyr::rename(time_value = target_date)

  autoplot(ewf, predictions,
    .color_by = .color_by, .facet_by = .facet_by,
    .base_color = .base_color, .max_facets = .max_facets
  )
}

starts_with_impl <- function(x, vars) {
  n <- nchar(x)
  x == substr(vars, 1, n)
}

plot_bands <- function(
    base_plot, predictions,
    levels = c(.5, .8, .95),
    fill = "blue4",
    alpha = 0.6,
    linewidth = 0.05) {
  innames <- names(predictions)
  n <- length(levels)
  alpha <- alpha / (n - 1)
  l <- (1 - levels) / 2
  l <- c(rev(l), 1 - l)

  ntarget_dates <- dplyr::n_distinct(predictions$time_value)

  predictions <- predictions %>%
    mutate(.pred_distn = dist_quantiles(quantile(.pred_distn, l), l)) %>%
    pivot_quantiles_wider(.pred_distn)
  qnames <- setdiff(names(predictions), innames)

  for (i in 1:n) {
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
