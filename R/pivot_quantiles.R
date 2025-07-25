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
#' pred_quantile <- quantile_pred(matrix(rnorm(20), 5), c(.2, .4, .6, .8))
#' nested_quantiles(pred_quantile)
#'
#' pred_quantile %>%
#'   as_tibble() %>%
#'   tidyr::nest(.by = .row) %>%
#'   dplyr::select(-.row)
#'
nested_quantiles <- function(x) {
  lifecycle::deprecate_warn("0.1.11", "nested_quantiles()", "hardhat::quantile_pred()")
  if (inherits(x, "quantile_pred")) {
    return(x %>% as_tibble() %>% tidyr::nest(.by = .row) %>%
      dplyr::select(data))
  }
  cli_abort(
    "`nested_quantiles()` is deprecated. See {.fn hardhat::quantile_pred}."
  )
}


#' Pivot a column containing `quantile_pred` to explicit rows or columns
#'
#' Both functions expand a column of `quantile_pred`s into the separate
#' quantiles. Since each consists of a set of names (quantiles) and values,
#' these operate analogously with `pivot_wider` and `pivot_longer`.
#'
#' `piot_quantiles_wider` creates a new column for each `quantile_level`, with
#' the values as the corresponding quantile values.  When pivoting multiple
#' columns, the original column name will be used as a prefix.
#'
#' Similarly, `pivot_quantiles_longer` assigns the selected columns
#' `quantile_level`s in one column and the `value`s in another. If multiple
#' columns are selected, these will be prefixed with the column name.
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
#' @name pivot_quantiles
#'
#' @examples
#' d1 <- quantile_pred(rbind(1:3, 2:4), 1:3 / 4)
#' d2 <- quantile_pred(rbind(2:4, 3:5), 2:4 / 5)
#' tib <- tibble(g = c("a", "b"), d1 = d1, d2 = d2)
#'
#' pivot_quantiles_longer(tib, "d1")
#' pivot_quantiles_longer(tib, dplyr::ends_with("1"))
#' pivot_quantiles_longer(tib, d2)
#'
#' pivot_quantiles_wider(tib, "d1")
#' pivot_quantiles_wider(tib, dplyr::ends_with("2"))
#' pivot_quantiles_wider(tib, d2)
NULL


#' @rdname pivot_quantiles
#' @export
pivot_quantiles_longer <- function(.data, ...) {
  col <- validate_pivot_quantiles(.data, ...)
  .data$.row <- seq_len(vctrs::vec_size(.data))
  long_tib <- as_tibble(.data[[col]])
  .data <- select(.data, !all_of(col))
  names(long_tib)[1:2] <- c(glue::glue("{col}_value"), glue::glue("{col}_quantile_level"))
  left_join(.data, long_tib, by = ".row") %>%
    select(!.row)
}

#' @rdname pivot_quantiles
#' @export
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
  lifecycle::deprecate_stop("0.0.6", "pivot_quantiles()", "pivot_quantiles_wider()")
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
