#' Turn a vector of quantile distributions into a list-col
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. The recommended alternative is
#' [hardhat::quantile_pred()] with [tibble::as_tibble()]

#'
#' @param x a `distribution` containing `dist_quantiles`
#'
#' @return a list-col
#' @export
#'
#' @examples
#' .pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
#' nested_quantiles(.pred_quantile)
#'
#' .pred_quantile %>%
#'   as_tibble() %>%
#'   tidyr::nest(.by = .row) %>%
#'   dplyr::select(-.row)
#'
nested_quantiles <- function(x) {
  lifecycle::deprecate_warn("0.1.11", "nested_quantiles()", "hardhat::quantile_pred()")
  if (inherits(x, "distribution")) {
    if (requireNamespace("distributional")) {
      x <- vctrs::vec_data(x)
      return(distributional:::dist_apply(x, .f = function(z) {
        as_tibble(vctrs::vec_data(z)) %>%
          mutate(across(everything(), as.double)) %>%
          vctrs::list_of()
      }))
    } else {
      cli_abort(c(
        "`nested_quantiles()` is deprecated and the {.pkg distributional}",
        `!` = "package is not installed.",
        i = "See {.fn hardhat::quantile_pred}."
      ))
    }
  }
  if (inherits(x, "quantile_pred")) {
    return(x %>% as_tibble() %>% tidyr::nest(.by = .row) %>%
      dplyr::select(data))
  }
  cli_abort(c(
    "`nested_quantiles()` is deprecated. See {.fn hardhat::quantile_pred}."
  ))
}


#' Pivot a column containing `quantile_pred` longer
#'
#' A column that contains `quantile_pred` will be "lengthened" with
#' the quantile levels serving as 1 column and the values as another. If
#' multiple columns are selected, these will be prefixed with the column name.
#'
#' @param .data A data frame, or a data frame extension such as a tibble or
#'   epi_df.
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame. Note that only one variable
#'   can be selected for this operation.
#'
#' @return An object of the same class as `.data`.
#' @export
#'
#' @examples
#' d1 <- quantile_pred(rbind(1:3, 2:4), 1:3 / 4)
#' d2 <- quantile_pred(rbind(2:4, 3:5), 2:4 / 5)
#' tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)
#'
#' pivot_quantiles_longer(tib, "d1")
#' pivot_quantiles_longer(tib, dplyr::ends_with("1"))
#' pivot_quantiles_longer(tib, d2)
pivot_quantiles_longer <- function(.data, ...) {
  col <- validate_pivot_quantiles(.data, ...)
  .data$.row <- seq_len(vctrs::vec_size(.data))
  long_tib <- as_tibble(.data[[col]])
  .data <- select(.data, !all_of(col))
  names(long_tib)[1:2] <- c(glue::glue("{col}_value"), glue::glue("{col}_quantile_level"))
  left_join(.data, long_tib, by = ".row") %>%
    select(!.row)
}

#' Pivot a column containing `quantile_pred` wider
#'
#' Any selected columns that contain `quantile_pred` will be "widened" with
#' the "taus" (quantile) serving as names and the values in the data frame.
#' When pivoting multiple columns, the original column name will be used as
#' a prefix.
#'
#' @inheritParams pivot_quantiles_longer
#'
#' @return An object of the same class as `.data`
#' @export
#'
#' @examples
#' d1 <- quantile_pred(rbind(1:3, 2:4), 1:3 / 4)
#' d2 <- quantile_pred(rbind(2:4, 3:5), 2:4 / 5)
#' tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)
#'
#' pivot_quantiles_wider(tib, "d1")
#' pivot_quantiles_wider(tib, dplyr::ends_with("2"))
#' pivot_quantiles_wider(tib, d2)
pivot_quantiles_wider <- function(.data, ...) {
  col <- validate_pivot_quantiles(.data, ...)
  .data$.row <- seq_len(vctrs::vec_size(.data))
  wide_tib <- as_tibble(.data[[col]]) %>%
    tidyr::pivot_wider(names_from = .quantile_levels, values_from = .pred_quantile)
  .data <- select(.data, !all_of(col))
  left_join(.data, wide_tib, by = ".row") %>%
    select(!.row)
}

pivot_quantiles <- function(.data, ...) {
  msg <- c(
    "{.fn pivot_quantiles} was deprecated in {.pkg epipredict} 0.0.6",
    i = "Please use {.fn pivot_quantiles_wider} instead."
  )
  lifecycle::deprecate_stop(msg)
}

validate_pivot_quantiles <- function(.data, ..., call = caller_env()) {
  expr <- rlang::expr(c(...))
  cols <- names(tidyselect::eval_select(expr, .data))
  if (length(cols) > 1L) {
    cli_abort(
      "Only one column can be pivotted. Can not pivot all of: {.var {cols}}.",
      call = call
    )
  }
  if (!inherits(.data[[cols]], "quantile_pred")) {
    cli_abort(
      "{.var {cols}} is not {.cls `quantile_pred`}. Cannot pivot it.",
      call = call
    )
  }
  cols
}
