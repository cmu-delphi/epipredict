abbr_to_fips <- function(abbr) {
  fi <- dplyr::left_join(
    tibble::tibble(abbr = tolower(abbr)),
    state_census, by = "abbr") %>%
    dplyr::mutate(fips = as.character(fips), fips = case_when(
      fips == "0" ~ "US",
      nchar(fips) < 2L ~ paste0("0", fips),
      TRUE ~ fips
    )) %>%
    pull(.data$fips)
  names(fi) <- NULL
  fi
}

#' Format predictions for submission to FluSight forecast Hub
#'
#'
#'
#' @param object a data.frame of predictions or an object of class
#'   `canned_epipred` as created by, e.g., [arx_forecaster()]
#' @param ... <[`dynamic-dots`][rlang::dyn-dots]> Name = value pairs of constant
#'   columns (or mutations) to perform to the results.
#' @param .fcast_period
#'
#' @return A [tibble::tibble].
#' @export
#'
#' @examples
flusight_hub_formatter <- function(
    object, ...,
    .fcast_period = c("daily", "weekly")) {
  UseMethod("flusight_hub_formatter")
}

#' @export
flusight_hub_formatter.canned_epipred <- function(
    object, ...,
    .fcast_period = c("daily", "weekly")) {
  flusight_hub_formatter(object$predictions, ..., .fcast_period = .fcast_period)
}

#' @export
flusight_hub_formatter.data.frame <- function(
    object, ...,
    .fcast_period = c("daily", "weekly")) {
  required_names <- c(".pred", ".pred_distn", "forecast_date", "geo_value")
  optional_names <- c("ahead", "target_date")
  hardhat::validate_column_names(object, required_names)
  if (!any(optional_names %in% names(object))) {
    cli::cli_abort("At least one of {.val {optional_names}} must be present.")
  }

  dots <- enquos(..., .named = TRUE)
  names <- names(dots)

  object <- object %>%
    # combine the predictions and the distribution
    dplyr::mutate(.pred_distn = nested_quantiles(.pred_distn)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      .pred_distn = list(add_row(.pred_distn, q = .pred, tau = NA)),
      .pred = NULL
    ) %>%
    tidyr::unnest(.pred_distn) %>%
    # now we create the correct column names
    dplyr::rename(
      value = q,
      output_type_id = tau,
      reference_date = forecast_date
    ) %>%
    # convert to fips codes, and add any constant cols passed in ...
    dplyr::mutate(location = abbr_to_fips(tolower(geo_value)), geo_value = NULL, !!!dots)

  # create target_end_date / horizon, depending on what is available
  pp <- ifelse(match.arg(.fcast_period) == "daily", 1L, 7L)
  has_ahead <- charmatch("ahead", names(object))
  if ("target_date" %in% names(object) && !is.na(has_ahead)) {
    object <- object %>%
      dplyr::rename(
        target_end_date = target_date,
        horizon = !!names(object)[has_ahead]
      )
  } else if (!is.na(has_ahead)) { # ahead present, not target date
    object <- object %>%
      dplyr::rename(horizon = !!names(object)[has_ahead]) %>%
      dplyr::mutate(target_end_date =  horizon * pp + reference_date)
  } else { # target_date present, not ahead
    object <- object %>%
      dplyr::rename(target_end_date = target_date) %>%
      dplyr::mutate(horizon = as.integer((target_end_date - reference_date)) / pp)
  }
  object %>% dplyr::relocate(
    reference_date, horizon, target_end_date, location, output_type_id, value
  )
}
