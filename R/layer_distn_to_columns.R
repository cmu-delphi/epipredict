#' Converts distributional forecasts to point forecasts
#'
#' This function adds a postprocessing layer to extract a point forecast from
#' a distributional forecast. NOTE: With default arguments, this will remove
#' information, so one should usually call this AFTER `layer_quantile_distn()`
#' or set the `name` argument to something specific.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param type character. Either `mean` or `median`.
#' @param name character. The name for the output column. The default `NULL`
#'   will overwrite the `.pred` column, removing the distribution information.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor.
#' @export
#'
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, quantile_reg(tau = c(.25, .5, .75))) %>% fit(jhu)
#'
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f1 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_quantile_distn() %>% # puts the other quantiles in a different col
#'   layer_point_from_distn() %>% # mutate `.pred` to contain only a point prediction
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f1)
#'
#' p1 <- predict(wf1, latest)
#' p1
#'
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_point_from_distn() %>% # mutate `.pred` to contain only a point prediction
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- predict(wf2, latest)
#' p2
layer_distn_to_columns <- function(frosting,
                                   ...,
                                   prefix = NULL,
                                   id = rand_id("distn_to_columns")) {
  arg_is_chr_scalar(id)
  arg_is_chr_scalar(prefix, allow_null = TRUE)

  add_layer(
    frosting,
    layer_distn_to_columns_new(
      terms = rlang::enquos(...),
      prefix = prefix,
      id = id
    )
  )
}

layer_distn_to_columns_new <- function(terms, prefix, id) {
  layer("distn_to_columns",
        terms = terms,
        prefix = prefix,
        id = id)
}

#' @export
slather.layer_dist_quantiles_to_columns <-
  function(object, components, the_fit, the_recipe, ...) {

    columns <- tidyselect::eval_select(object$terms, components$predictions)
    if (length(columns) == 0L) {
      cli::cli_warn(
        c("`layer_distn_to_columns` did not result in any selections.",
          i = "Ignoring this layer."))
      return(components)
    }
    dist_columns <- rep(TRUE, length(columns))
    for (i in seq_along(columns)) {
      if (!is_dist_quantiles(components$predictions[[i]])) {
        nm <- columns[i]
        cli::cli_warn(
          c("{nm} is not a set of quantiles and cannot be converted to columns.",
            i = "Ignoring this column."))
        dist_columns[i] <- FALSE
      }
    }
    columns <- columns[dist_columns]
    if (length(columns) == 0L) {
      cli::cli_warn(
        c("`In `layer_dist_quantiles_to_columns()`, no selections were `dist_quantiles`.",
          i = "Perhaps you meant to first call `layer_quantile_distn()`?",
          i = "Or `layer_residual_quantiles()`?",
          i = "Ignoring this layer."))
      return(components)
    }

    components$predictions <- components$predictions %>%
      pivot_quantiles(tidyselect::all_of(columns))

    components
  }

#' Pivot columns containing `dist_quantile` wider
#'
#' Any selected columns that contain `dist_quantiles` will be "widened" with
#' the "taus" (quantile) serving as names and the values in the data frame.
#' When pivoting multiple columns, the original column name will be used as
#' a prefix.
#'
#' @param .data A data frame, or a data frame extension such as a tibble or
#'   epi_df.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables. Any selected columns should
#'
#' @return An object of the same class as `.data`
#' @export
#'
#' @examples
#' d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 1:3 / 4))
#' d2 <- c(dist_quantiles(2:4, 2:4 / 5), dist_quantiles(3:5, 2:4 / 5))
#' tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)
#'
#' pivot_quantiles(tib, c("d1", "d2"))
#' pivot_quantiles(tib, tidyselect::starts_with("d"))
#' pivot_quantiles(tib, d2)
pivot_quantiles <- function(.data, ...) {
  expr <- rlang::expr(c(...))
  cols <- names(tidyselect::eval_select(expr, .data))
  dqs <- map_lgl(cols, ~ is_dist_quantiles(.data[[.x]]))
  if (!all(dqs)) {
    nms <- cols[!dqs]
    cli::cli_abort("{nms} are not `dist_quantiles. Cannot pivot them.")
  }
  .data <- .data %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(cols), nested_quantiles))
  checks <- map_lgl(cols, ~ diff(range(map_int(.data[[.x]], length))) == 0L)
  if (!all(checks)) {
    nms <- cols[!checks]
    cli::cli_abort(
      c("Quantiles must be the same length and have the same set of `taus`.",
        i = "Check failed for column(s) {nms}."))
  }
  if (length(cols) > 1L) {
    for (col in cols) {
      .data <- .data %>%
        tidyr::unnest(tidyselect::all_of(col)) %>%
        tidyr::pivot_wider(
          names_from = "tau", values_from = "q",
          names_prefix = paste0(col, "_")
        )
    }
  } else {
    .data <- .data %>%
      tidyr::unnest(tidyselect::all_of(cols)) %>%
      tidyr::pivot_wider(names_from = "tau", values_from = "q")
  }

  .data
}


#' @export
print.layer_point_from_distn <- function(
    x, width = max(20, options()$width - 30), ...) {

  title <- "Extracting point predictions"
  if (is.null(x$name)) {
    cnj <- NULL
    ext <- "<overwriting .pred>"
  } else {
    cnj <- "adding column"
    ext <- x$name
  }
  print_layer(title = title, width = width, conjunction = cnj, extra_text = ext)
}

