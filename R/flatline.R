#' (Internal) implementation of the flatline forecaster
#'
#' This is an internal function that is used to create a [parsnip::linear_reg()]
#' model. It has somewhat odd behaviour (see below).
#'
#'
#' @param formula The lhs should be a single variable. In standard usage, this
#'   would actually be the observed time series shifted forward by the forecast
#'   horizon. The right hand side must contain any keys (locations) for the
#'   panel data separated by plus. The observed time series must come last.
#'   For example
#'   ```r
#'   form <- as.formula(lead7_y ~ state + age + y)
#'   ```
#'   Note that this function doesn't DO the shifting, that has to be done
#'   outside.
#' @param data A data frame containing at least the variables used in the
#'   formula. It must also contain a column `time_value` giving the observed
#'   time points.
#'
#' @return An S3 object of class `flatline` with two components:
#' * `residuals` - a tibble with all keys and a `.resid` column that contains
#'   forecast errors.
#' * `.pred` - a tibble with all keys and a `.pred` column containing only
#'   predictions for future data (the last observed of the outcome for each
#'   combination of keys.
#' @export
#' @keywords internal
#'
#' @examples
#' tib <- data.frame(
#'   y = runif(100),
#'   expand.grid(k = letters[1:4], j = letters[5:9], time_value = 1:5)
#' ) %>%
#'   dplyr::group_by(k, j) %>%
#'   dplyr::mutate(y2 = dplyr::lead(y, 2)) # predict 2 steps ahead
#' flat <- flatline(y2 ~ j + k + y, tib) # predictions for 20 locations
#' sum(!is.na(flat$residuals$.resid)) # 100 residuals, but 40 are NA
flatline <- function(formula, data) {
  response <- recipes:::get_lhs_vars(formula, data)
  rhs <- recipes:::get_rhs_vars(formula, data)
  n <- length(rhs)
  observed <- rhs[n] # DANGER!!
  ek <- rhs[-n]
  if (length(response) > 1) {
    cli_abort("flatline forecaster can accept only 1 observed time series.")
  }
  keys <- kill_time_value(ek)

  preds <- data %>%
    mutate(
      .pred = !!rlang::sym(observed),
      .resid = !!rlang::sym(response) - .pred
    )
  .pred <- preds %>%
    filter(!is.na(.pred)) %>%
    group_by(!!!rlang::syms(keys)) %>%
    arrange(time_value) %>%
    dplyr::slice_tail(n = 1L) %>%
    ungroup() %>%
    select(all_of(c(keys, ".pred")))

  structure(
    list(
      residuals = select(preds, all_of(c(keys, ".resid"))),
      .pred = .pred
    ),
    class = "flatline"
  )
}

#' @export
residuals.flatline <- function(object, ...) {
  object$residuals
}

#' @export
predict.flatline <- function(object, newdata, ...) {
  object <- object$.pred
  metadata <- names(object)[names(object) != ".pred"]
  ek <- names(newdata)
  if (!all(metadata %in% ek)) {
    cli_abort(c(
      "`newdata` has different metadata than was used",
      "to fit the flatline forecaster"
    ))
  }

  left_join(newdata, object, by = metadata)$.pred
}

#' @export
print.flatline <- function(x, ...) {
  keys <- colnames(x$.pred)
  keys <- paste(keys[!(keys %in% ".pred")], collapse = ", ")
  nloc <- nrow(x$.pred)
  nres <- nrow(x$residuals)
  pmsg <- glue::glue(
    "Predictions produced by {keys} resulting in {nloc} total forecasts."
  )
  rmsg <- glue::glue(
    "A total of {nres} residuals are available from the training set."
  )
  cat("Flatline forecaster\n")
  cat("\n")
  cat(pmsg)
  cat("\n")
  cat(rmsg)
  cat("\n\n")
}
