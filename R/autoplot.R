#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot

#' @export
autoplot.epi_workflow <- function(
    object, predictions = NULL,
    .levels = c(.5, .8, .95),
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "#3A448F",
    .pred_line_color = "orange",
    .max_facets = Inf) {
  arg_is_probabilities(.levels)
  if (!workflows::is_trained_workflow(object)) {
    cli::cli_abort(c(
      "Can't plot an untrained {.cls epi_workflow}.",
      i = "Do you need to call `fit()`?"
    ))
  }

  mold <- workflows::extract_mold(object)
  y <- mold$outcomes
  if (ncol(y) > 1) {
    y <- y[, 1]
    cli::cli_warn("Multiple outcome variables were detected. Displaying only 1.")
  }
  keys <- c("time_value", "geo_value", "key")
  mold_roles <- names(mold$extras$roles)
  edf <- dplyr::bind_cols(mold$extras$roles[mold_roles %in% keys], y)
  if (starts_with_impl("ahead_", names(y))) {
    old_name_y <- unlist(strsplit(names(y), "_"))
    shift <- -as.numeric(old_name_y[2])
    new_name_y <- paste(old_name_y[-c(1:2)], collapse = "_")
    edf <- dplyr::rename(edf, !!new_name_y := !!names(y))
  } else if (starts_with_impl("lag_", names(y))) {
    old_name_y <- unlist(strsplit(names(y), "_"))
    shift <- as.numeric(old_name_y[2])
    new_name_y <- paste(old_name_y[-c(1:2)], collapse = "_")
    edf <- dplyr::rename(edf, !!new_name_y := !!names(y))
  }

  if (!is.null(shift)) {
    edf <- dplyr::mutate(edf, time_value = time_value + shift)
  }
  extra_keys <- setdiff(epi_keys_mold(mold), c("time_value", "geo_value"))
  if (length(extra_keys) == 0L) extra_keys <- NULL
  edf <- as_epi_df(edf,
    as_of = wf$fit$meta$as_of,
    additional_metadata = list(other_keys = extra_keys)
  )
  if (is.null(predictions)) {
    return(autoplot(
      edf, new_name_y,
      .color_by = .colour_by, .facet_by = .facet_by, .base_color = .base_color,
      .max_facets = .max_facets
    ))
  }

  pred_cols_ok <- hardhat::check_column_names(predictions, epi_keys(edf))
  if (!pred_cols_ok$ok) {
    cli::cli_warn(c(
      "`predictions` is missing required variables: {.var {pred_cols_ok$missing_names}}.",
      i = "Plotting the original data."
    ))
    return(autoplot(
      edf, !!new_name_y,
      .color_by = .colour_by, .facet_by = .facet_by, .base_color = .base_color,
      .max_facets = .max_facets
    ))
  }

  # First we plot the history, always faceted by everything
  bp <- autoplot(edf, !!new_name_y,
    .color_by = "none", .facet_by = "all_keys",
    .base_color = "black", .max_facets = .max_facets
  )

  # Now, prepare matching facets in the predictions
  ek <- kill_time_value(epi_keys(edf))
  predictions <- predictions %>%
    dplyr::mutate(
      .facets = interaction(!!!rlang::syms(as.list(ek)), sep = "/"),
    )
  if (.max_facets < Inf) {
    top_n <- levels(as.factor(bp$data$.facets))[seq_len(.max_facets)]
    predictions <- dplyr::filter(predictions, .facets %in% top_n) %>%
      dplyr::mutate(.facets = droplevels(.facets))
  }


  if (".pred_distn" %in% names(predictions)) {
    bp <- plot_bands(bp, predictions, .levels, .base_color)
  }

  if (".pred" %in% names(predictions)) {
    ntarget_dates <- dplyr::n_distinct(predictions$time_value)
    if (ntarget_dates > 1L) {
      bp <- bp +
        ggplot2::geom_line(
          data = predictions, aes(y = .data$.pred),
          color = .pred_line_color
        )
    } else {
      bp <- bp +
        ggplot2::geom_point(
          data = predictions, aes(y = .data$.pred),
          color = .pred_line_color
        )
    }
  }
  bp
}

#' @export
autoplot.epi_df <- function(
    object, ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "#3A448F",
    .max_facets = Inf) {
  .color_by <- match.arg(.color_by)
  .facet_by <- match.arg(.facet_by)

  arg_is_pos(.max_facets)
  arg_is_scalar(.max_facets)
  if (is.finite(.max_facets)) arg_is_int(.max_facets)
  arg_is_chr_scalar(.base_color)

  ek <- epi_keys(object)
  mv <- setdiff(names(object), ek)
  ek <- kill_time_value(ek)

  # --- check for numeric variables
  allowed <- map_lgl(object[mv], is.numeric)
  if (length(allowed) == 0) {
    cli::cli_abort("No numeric variables were available to plot automatically.")
  }
  vars <- tidyselect::eval_select(rlang::expr(c(...)), object)
  if (rlang::is_empty(vars)) { # find them automatically if unspecified
    vars <- tidyselect::eval_select(names(allowed)[1], object)
    cli::cli_warn(
      "Plot variable was unspecified. Automatically selecting {.var {names(allowed)[1]}}."
    )
  } else { # if variables were specified, ensure that they are numeric
    ok <- names(vars) %in% names(allowed)
    if (!any(ok)) {
      cli::cli_abort(
        "None of the requested variables {.var {names(vars)}} are numeric."
      )
    } else if (!all(ok)) {
      cli::cli_warn(c(
        "Only the requested variables {.var {names(vars)[ok]}} are numeric.",
        i = "`autoplot()` cannot display {.var {names(vars)[!ok]}}."
      ))
      vars <- vars[ok]
    }
  }

  # --- create a viable df to plot
  pos <- tidyselect::eval_select(
    rlang::expr(c("time_value", ek, names(vars))), object
  )
  if (length(vars) > 1) {
    object <- tidyr::pivot_longer(
      object[pos], tidyselect::all_of(names(vars)),
      values_to = ".response",
      names_to = ".response_name"
    )
  } else {
    object <- dplyr::rename(object[pos], .response := !!names(vars))
  }
  all_keys <- rlang::syms(as.list(ek))
  other_keys <- rlang::syms(as.list(setdiff(ek, "geo_value")))
  all_avail <- rlang::syms(as.list(c(ek, ".response_name")))

  object <- object %>%
    dplyr::mutate(
      .colours = switch(.color_by,
        all_keys = interaction(!!!all_keys, sep = "/"),
        geo_value = geo_value,
        other_keys = interaction(!!!other_keys, sep = "/"),
        all = interaction(!!!all_avail, sep = "/"),
        NULL
      ),
      .facets = switch(.facet_by,
        all_keys = interaction(!!!all_keys, sep = "/"),
        geo_value = as.factor(geo_value),
        other_keys = interaction(!!!other_keys, sep = "/"),
        all = interaction(!!!all_avail, sep = "/"),
        NULL
      )
    )

  if (.max_facets < Inf && ".facets" %in% names(object)) {
    n_facets <- nlevels(object$.facets)
    if (n_facets > .max_facets) {
      top_n <- levels(as.factor(object$.facets))[seq_len(.max_facets)]
      object <- dplyr::filter(object, .facets %in% top_n) %>%
        dplyr::mutate(.facets = droplevels(.facets))
      if (".colours" %in% names(object)) {
        object <- dplyr::mutate(object, .colours = droplevels(.colours))
      }
    }
  }

  p <- ggplot2::ggplot(
    object,
    ggplot2::aes(x = .data$time_value)
  ) +
    ggplot2::theme_bw()

  if (".colours" %in% names(object)) {
    p <- p + ggplot2::geom_line(
      ggplot2::aes(y = .data$.response, colour = .data$.colours),
      key_glyph = "timeseries"
    ) +
      ggplot2::scale_colour_viridis_d(name = "")
  } else if (length(vars) > 1 && .color_by == ".response") {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(
        y = .data$.response, colour = .data$.response_name
      )) +
      ggplot2::scale_colour_viridis_d(name = "")
  } else { # none
    p <- p +
      ggplot2::geom_line(ggplot2::aes(y = .data$.response), color = .base_color)
  }

  if (".facets" %in% names(object)) {
    p <- p + ggplot2::facet_wrap(~.facets, scales = "free_y") +
      ggplot2::ylab(names(vars))
    if (.facet_by == "all") p <- p + ggplot2::ylab("")
  } else if ((length(vars) > 1 && .facet_by == ".response")) {
    p <- p + ggplot2::facet_wrap(~.response_name, scales = "free_y") +
      ggplot2::ylab("")
  } else {
    p <- p + ggplot2::ylab(names(vars))
  }
  p
}

#' @export
autoplot.canned_epipred <- function(
    object, ...,
    .color_by = c("all_keys", "geo_value", "other_keys", ".response", "all", "none"),
    .facet_by = c(".response", "other_keys", "all_keys", "geo_value", "all", "none"),
    .base_color = "#3A448F",
    .max_facets = Inf) {
  rlang::check_dots_empty()
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
    fill = "#3A448F",
    alpha = 0.6,
    linewidth = 0.05) {
  innames <- names(predictions)
  n <- length(levels)
  alpha <- alpha / (n - 1)
  l <- (1 - levels) / 2
  l <- c(rev(l), 1 - l)

  ntarget_dates <- dplyr::n_distinct(predictions$time_value)

  predictions <- predictions %>%
    dplyr::mutate(.pred_distn = dist_quantiles(quantile(.pred_distn, l), l)) %>%
    pivot_quantiles_wider(.pred_distn)
  qnames <- setdiff(names(predictions), innames)

  for (i in 1:n) {
    bottom <- qnames[i]
    top <- rev(qnames)[i]
    if (i == 1) {
      if (ntarget_dates > 1L) {
        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            data = predictions,
            ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
            alpha = 0.2, linewidth = linewidth, fill = fill
          )
      } else {
        base_plot <- base_plot +
          ggplot2::geom_linerange(
            data = predictions,
            ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
            alpha = 0.2, linewidth = 2, color = fill
          )
      }
    } else {
      if (ntarget_dates > 1L) {
        base_plot <- base_plot +
          ggplot2::geom_ribbon(
            data = predictions,
            ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
            fill = fill, alpha = alpha
          )
      } else {
        base_plot <- base_plot +
          ggplot2::geom_linerange(
            data = predictions,
            ggplot2::aes(ymin = .data[[bottom]], ymax = .data[[top]]),
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
