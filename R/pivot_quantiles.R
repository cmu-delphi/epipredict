#' Turn a vector of quantile distributions into a list-col
#'
#' @param x a `distribution` containing `dist_quantiles`
#'
#' @return a list-col
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' edf <- case_death_rate_subset[1:3, ]
#' edf$q <- dist_quantiles(list(1:5, 2:4, 3:10), list(1:5 / 6, 2:4 / 5, 3:10 / 11))
#'
#' edf_nested <- edf %>% mutate(q = nested_quantiles(q))
#' edf_nested %>% unnest(q)
nested_quantiles <- function(x) {
  stopifnot(is_dist_quantiles(x))
  distributional:::dist_apply(x, .f = function(z) {
    as_tibble(vec_data(z)) %>%
      mutate(across(everything(), as.double)) %>%
      vctrs::list_of()
  })
}


#' Pivot columns containing `dist_quantile` longer
#'
#' Selected columns that contain `dist_quantiles` will be "lengthened" with
#' the quantile levels serving as 1 column and the values as another. If
#' multiple columns are selected, these will be prefixed with the column name.
#'
#' @param .data A data frame, or a data frame extension such as a tibble or
#'   epi_df.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables.
#' @param .ignore_length_check If multiple columns are selected, as long as
#'   each row has contains the same number of quantiles, the result will be
#'   reasonable. But if, for example, `var1[1]` has 5 quantiles while `var2[1]`
#'   has 7, then the only option would be to recycle everything, creating a
#'   _very_ long result. By default, this would throw an error. But if this is
#'   really the goal, then the error can be bypassed by setting this argument
#'   to `TRUE`. The quantiles in the first selected column will vary the fastest.
#'
#' @return An object of the same class as `.data`.
#' @export
#'
#' @examples
#' d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 1:3 / 4))
#' d2 <- c(dist_quantiles(2:4, 2:4 / 5), dist_quantiles(3:5, 2:4 / 5))
#' tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)
#'
#' pivot_quantiles_longer(tib, "d1")
#' pivot_quantiles_longer(tib, dplyr::ends_with("1"))
#' pivot_quantiles_longer(tib, d1, d2)
pivot_quantiles_longer <- function(.data, ..., .ignore_length_check = FALSE) {
  cols <- validate_pivot_quantiles(.data, ...)
  .data <- .data %>% mutate(across(all_of(cols), nested_quantiles))
  if (length(cols) > 1L) {
    lengths_check <- .data %>%
      dplyr::transmute(across(all_of(cols), ~ map_int(.x, vctrs::vec_size))) %>%
      as.matrix() %>%
      apply(1, function(x) dplyr::n_distinct(x) == 1L) %>%
      all()
    if (lengths_check) {
      .data <- tidyr::unnest(.data, all_of(cols), names_sep = "_")
    } else {
      if (.ignore_length_check) {
        for (col in cols) {
          .data <- .data %>% tidyr::unnest(all_of(col), names_sep = "_")
        }
      } else {
        cli::cli_abort(paste(
          "Some selected columns contain different numbers of quantiles.",
          "The result would be a {.emph very} long {.cls tibble}.",
          "To do this anyway, rerun with `.ignore_length_check = TRUE`."
        ))
      }
    }
  } else {
    .data <- .data %>% tidyr::unnest(all_of(cols))
  }
  .data
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
#'   be used to select a range of variables.
#'
#' @return An object of the same class as `.data`
#' @export
#'
#' @examples
#' d1 <- c(dist_quantiles(1:3, 1:3 / 4), dist_quantiles(2:4, 1:3 / 4))
#' d2 <- c(dist_quantiles(2:4, 2:4 / 5), dist_quantiles(3:5, 2:4 / 5))
#' tib <- tibble::tibble(g = c("a", "b"), d1 = d1, d2 = d2)
#'
#' pivot_quantiles_wider(tib, c("d1", "d2"))
#' pivot_quantiles_wider(tib, dplyr::starts_with("d"))
#' pivot_quantiles_wider(tib, d2)
pivot_quantiles_wider <- function(.data, ...) {
  cols <- validate_pivot_quantiles(.data, ...)
  .data <- .data %>% mutate(across(all_of(cols), nested_quantiles))
  checks <- map_lgl(cols, ~ diff(range(vctrs::list_sizes(.data[[.x]]))) == 0L)
  if (!all(checks)) {
    nms <- cols[!checks]
    cli::cli_abort(c(
      "Quantiles must be the same length and have the same set of taus.",
      i = "Check failed for variables(s) {.var {nms}}."
    ))
  }

  # tidyr::pivot_wider can crash if there are duplicates, this generally won't
  # happen in our context. To avoid, silently add an index column and remove it
  # later
  .hidden_index <- seq_len(nrow(.data))
  .data <- tibble::add_column(.data, .hidden_index = .hidden_index)
  if (length(cols) > 1L) {
    for (col in cols) {
      .data <- .data %>%
        tidyr::unnest(all_of(col)) %>%
        tidyr::pivot_wider(
          names_from = "quantile_levels", values_from = "values",
          names_prefix = paste0(col, "_")
        )
    }
  } else {
    .data <- .data %>%
      tidyr::unnest(all_of(cols)) %>%
      tidyr::pivot_wider(names_from = "quantile_levels", values_from = "values")
  }
  select(.data, -.hidden_index)
}

pivot_quantiles <- function(.data, ...) {
  msg <- c(
    "{.fn pivot_quantiles} was deprecated in {.pkg epipredict} 0.0.6",
    i = "Please use {.fn pivot_quantiles_wider} instead."
  )
  lifecycle::deprecate_stop(msg)
}

validate_pivot_quantiles <- function(.data, ...) {
  expr <- rlang::expr(c(...))
  cols <- names(tidyselect::eval_select(expr, .data))
  dqs <- map_lgl(cols, ~ is_dist_quantiles(.data[[.x]]))
  if (!all(dqs)) {
    nms <- cols[!dqs]
    cli::cli_abort(
      "Variables(s) {.var {nms}} are not `dist_quantiles`. Cannot pivot them."
    )
  }
  cols
}
