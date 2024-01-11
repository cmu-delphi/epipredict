#' Tidy the result of a frosting object
#'
#' `tidy` will return a data frame that contains information
#'  regarding a frosting or operation within the frosting (when a `tidy`
#'  method for the operation exists). Note that this is a modified
#'  version of the `tidy` method for a recipe.
#'
#' @name tidy.frosting
#'
#' @param x A `frosting` or `layer` object
#' @param number An integer or `NA`. If missing, and `id` is not provided,
#'  the return value is a list of the operations in the frosting.
#'  If a number is given, a `tidy` method is executed for that operation
#'  in the frosting (if it exists). `number` must not be provided if
#'  `id` is.
#' @param id A character string or `NA`. If missing and `number` is not provided,
#'  the return value is a list of the operations in the frosting.
#'  If a character string is given, a `tidy` method is executed for that
#'  operation in the frosting (if it exists). `id` must not be provided
#'  if `number` is.
#' @param ... Not currently used.
#' @return A tibble with columns that vary depending on what
#'  `tidy` method is executed. When `number`, and `id` are `NA`, a
#'  tibble with columns `number` (the operation iteration),
#'  `operation` ("layer"),
#'  `type` (the method, e.g. "predict", "naomit"), and a character column `id`.
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
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#' latest <- get_test_data(recipe = r, x = jhu)

#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_naomit(.pred)
#'
#' tidy(f)
#'
#' @rdname tidy.frosting
#' @export
tidy.frosting <- function(x, number = NA, id = NA, ...) {
  # add id = NA as default. If both ID & number are non-NA, error.
  # If number is NA and ID is not, select the layer with the corresponding
  # ID. Only a single ID is allowed, as this follows the convention for number
  rlang::check_dots_empty()
  num_oper <- length(x$layers)
  pattern <- "^layer_"

  if (length(id) != 1L) {
    rlang::abort("If `id` is provided, it must be a length 1 character vector.")
  }

  if (length(number) != 1L) {
    rlang::abort("If `number` is provided, it must be a length 1 integer vector.")
  }

  if (!is.na(id)) {
    if (!is.na(number)) {
      rlang::abort("You may specify `number` or `id`, but not both.")
    }
    layer_ids <- vapply(x$layers, function(x) x$id, character(1))
    if (!(id %in% layer_ids)) {
      rlang::abort("Supplied `id` not found in the frosting.")
    }
    number <- which(id == layer_ids)
  }
  if (is.na(number)) {
    ids <- vapply(x$layers, function(x) x$id, character(1))

    oper_classes <- lapply(x$layers, class)
    oper_classes <- grep("_", unlist(oper_classes), value = TRUE)

    oper <- strsplit(oper_classes, split = "_")
    oper <- vapply(oper, function(x) x[1], character(1))

    oper_types <- gsub(pattern, "", oper_classes)

    res <- tibble(
      number = seq_along(x$layers),
      operation = oper,
      type = oper_types,
      id = ids
    )
  } else {
    if (number > num_oper || length(number) > 1) {
      rlang::abort(
        paste0(
          "`number` should be a single value between 1 and ",
          num_oper,
          "."
        )
      )
    }

    res <- tidy(x$layers[[number]], ...)
  }
  res
}

#' @export
tidy.layer <- function(x, ...) {
  cli::cli_abort(
    "No `tidy()` method exists for a layer with class: {.cls {class(x)}}."
  )
}
