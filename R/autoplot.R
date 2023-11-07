
#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


autoplot.epi_workflow <- function(object, ..., .max_keys = Inf) {
  if (!workflows::is_trained_workflow(object)) {
    cli::cli_abort(c(
      "Can't plot an untrained {.cls epi_workflow}.",
      i = "Do you need to call `fit()`?"
    ))
  }

  mold <- workflows::extract_mold(object)
  y <- mold$outcomes
  if (ncol(y) > 1) {
    y <- y[,1]
    cli::cli_warn("Multiple outcome variables were detected. Displaying only 1.")
  }
  edf <- dplyr::bind_cols(mold$extras, y)
  if (starts_with_impl("ahead_", names(y))) {
    new_name_y <- unlist(strsplit(names(y), "_"))
    shift <- -as.numeric(new_name_y[2])
    edf <- dplyr::rename(edf, !!new_name_y[3] := !!names(y))
  } else if (starts_with_impl("lag_", names(y))) {
    new_name_y <- unlist(strsplit(names(y), "_"))
    shift <- as.numeric(new_name_y[2])
    edf <- dplyr::rename(edf, !!new_name_y[3] := !!names(y))
  }

  if (!is.null(shift)) {
    edf <- dplyr::mutate(edf, time_value = time_value + shift_back)
  }
  autoplot(edf, ..., .max_keys = .max_keys)
}

#' @export
autoplot.epi_df <- function(object, ..., .max_keys = Inf) {
  ek <- epi_keys(object)
  mv <- setdiff(names(object), ek)
  ek <- kill_time_value(ek)
  allowed <- map_lgl(object[mv], is.numeric)
  if (length(allowed) == 0) {
    cli::cli_abort("No numeric variables were available to plot automatically.")
  }
  vars <- tidyselect::eval_select(rlang::expr(c(...)), object)
  if (rlang::is_empty(vars)) {
    vars <- tidyselect::eval_select(names(allowed)[1], object)
    cli::cli_warn(
      "Plot variable was unspecified. Automatically selecting {.var {names(allowed)[1]}}."
    )
  } else {
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
  pos <- tidyselect::eval_select(
    rlang::expr(c("time_value", ek, names(vars))), object
  )
  if (length(vars) > 1) {
    object <- tidyr::pivot_longer(
      object[pos], tidyselect::all_of(names(vars)),
      values_to = ".response"
    )
  } else {
    object <- dplyr::rename(object[pos], .response := !!names(vars))
  }

  cc <- rlang::expr(interaction(!!!rlang::syms(as.list(ek)), sep = "/"))
  p <- ggplot2::ggplot(
    object,
    ggplot2::aes(x = .data$time_value, y = .data$.response)
  ) +
    ggplot2::geom_line(
      ggplot2::aes(colour = !!cc),
      key_glyph = "timeseries"
    ) +
    ggplot2::scale_colour_viridis_d(name = "") +
    ggplot2::theme_bw()

  if (length(vars) > 1) {
    p <- p + ggplot2::facet_wrap(~name, scales = "free_y") + ggplot2::ylab("")
  } else {
    p <- p + ggplot2::ylab(names(vars))
  }
  p
}

starts_with_impl <- function(x, vars) {
  n <- nchar(x)
  which(x == substr(vars, 1, n))
}
