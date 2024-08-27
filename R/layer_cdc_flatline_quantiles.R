#' CDC Flatline Forecast Quantiles
#'
#' This layer creates quantile forecasts by taking a sample from the
#' interpolated CDF of the flatline residuals, and shuffling them.
#' These are then added on to the point prediction.
#'
#' @details
#' This layer is intended to be used in concert with [flatline()]. But it can
#' also be used with anything else. As long as residuals are available in the
#' the fitted model, this layer could be useful. Like
#' [layer_residual_quantiles()] it only uses the residuals for the fitted model
#' object. However, it propagates these forward for *all* aheads, by
#' iteratively shuffling them (randomly), and then adding them to the previous
#' set. This is in contrast to what happens with the [flatline_forecaster()].
#' When using [flatline()] as the underlying engine (here), both will result in the
#' same predictions (the most recent observed value), but that model calculates
#' separate residuals for each `ahead` by comparing to observations further into
#' the future. This version continues to use the same set of residuals, and
#' adds them on to produce wider intervals as `ahead` increases.
#'
#'
#' @inheritParams layer_residual_quantiles
#' @param aheads Numeric vector of desired forecast horizons. These should be
#'   given in the "units of the training data". So, for example, for data
#'   typically observed daily (possibly with missing values), but with weekly
#'   forecast targets, you would use `c(7, 14, 21, 28)`. But with weekly data,
#'   you would use `1:4`.
#' @param quantile_levels Numeric vector of probabilities with values in (0,1)
#'   referring to the desired predictive intervals. The default is the standard
#'   set for the COVID Forecast Hub.
#' @param nsims Positive integer. The number of draws from the empirical CDF.
#'   These samples are spaced evenly on the (0, 1) scale, F_X(x) resulting in
#'   linear interpolation on the X scale. This is achieved with
#'   [stats::quantile()] Type 7 (the default for that function).
#' @param symmetrize Scalar logical. If `TRUE`, does two things: (i) forces the
#'   "empirical" CDF of residuals to be symmetric by pretending that for every
#'   actually-observed residual X we also observed another residual -X, and (ii)
#'   at each ahead, forces the median simulated value to be equal to the point
#'   prediction by adding or subtracting the same amount to every simulated
#'   value. Adjustments in (ii) take place before propagating forward and
#'   simulating the next ahead. This forces any 1-ahead predictive intervals to
#'   be symmetric about the point prediction, and encourages larger aheads to be
#'   more symmetric.
#' @param nonneg Scalar logical. Force all predictive intervals be non-negative.
#'   Because non-negativity is forced _before_ propagating forward, this has
#'   slightly different behaviour than would occur if using [layer_threshold()].
#'   Thresholding at each ahead takes place after any shifting from
#'   `symmetrize`.
#'
#' @return an updated `frosting` postprocessor. Calling [predict()] will result
#'   in an additional `<list-col>` named `.pred_distn_all` containing 2-column
#'   [tibble::tibble()]'s. For each
#'   desired combination of `key`'s, the tibble will contain one row per ahead
#'   with the associated [dist_quantiles()].
#' @export
#'
#' @examples
#' r <- epi_recipe(case_death_rate_subset) %>%
#'   # data is "daily", so we fit this to 1 ahead, the result will contain
#'   # 1 day ahead residuals
#'   step_epi_ahead(death_rate, ahead = 1L, skip = TRUE) %>%
#'   recipes::update_role(death_rate, new_role = "predictor") %>%
#'   recipes::add_role(time_value, geo_value, new_role = "predictor")
#'
#' forecast_date <- max(case_death_rate_subset$time_value)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_cdc_flatline_quantiles(aheads = c(7, 14, 21, 28), symmetrize = TRUE)
#'
#' eng <- parsnip::linear_reg() %>% parsnip::set_engine("flatline")
#'
#' wf <- epi_workflow(r, eng, f) %>% fit(case_death_rate_subset)
#' preds <- forecast(wf) %>%
#'   dplyr::select(-time_value) %>%
#'   dplyr::mutate(forecast_date = forecast_date)
#' preds
#'
#' preds <- preds %>%
#'   unnest(.pred_distn_all) %>%
#'   pivot_quantiles_wider(.pred_distn) %>%
#'   mutate(target_date = forecast_date + ahead)
#'
#' if (require("ggplot2")) {
#'   four_states <- c("ca", "pa", "wa", "ny")
#'   preds %>%
#'     filter(geo_value %in% four_states) %>%
#'     ggplot(aes(target_date)) +
#'     geom_ribbon(aes(ymin = `0.1`, ymax = `0.9`), fill = blues9[3]) +
#'     geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), fill = blues9[6]) +
#'     geom_line(aes(y = .pred), color = "orange") +
#'     geom_line(
#'       data = case_death_rate_subset %>% filter(geo_value %in% four_states),
#'       aes(x = time_value, y = death_rate)
#'     ) +
#'     scale_x_date(limits = c(forecast_date - 90, forecast_date + 30)) +
#'     labs(x = "Date", y = "Death rate") +
#'     facet_wrap(~geo_value, scales = "free_y") +
#'     theme_bw() +
#'     geom_vline(xintercept = forecast_date)
#' }
layer_cdc_flatline_quantiles <- function(
    frosting,
    ...,
    aheads = 1:4,
    quantile_levels = c(.01, .025, 1:19 / 20, .975, .99),
    nsims = 1e3,
    by_key = "geo_value",
    symmetrize = FALSE,
    nonneg = TRUE,
    id = rand_id("cdc_baseline_bands")) {
  rlang::check_dots_empty()

  arg_is_int(aheads)
  arg_is_probabilities(quantile_levels, allow_null = TRUE)
  arg_is_pos_int(nsims)
  arg_is_scalar(nsims)
  arg_is_chr_scalar(id)
  arg_is_lgl_scalar(symmetrize, nonneg)
  arg_is_chr(by_key, allow_null = TRUE, allow_na = TRUE, allow_empty = TRUE)

  add_layer(
    frosting,
    layer_cdc_flatline_quantiles_new(
      aheads = aheads,
      quantile_levels = quantile_levels,
      nsims = nsims,
      by_key = by_key,
      symmetrize = symmetrize,
      nonneg = nonneg,
      id = id
    )
  )
}

layer_cdc_flatline_quantiles_new <- function(
    aheads,
    quantile_levels,
    nsims,
    by_key,
    symmetrize,
    nonneg,
    id) {
  layer(
    "cdc_flatline_quantiles",
    aheads = aheads,
    quantile_levels = quantile_levels,
    nsims = nsims,
    by_key = by_key,
    symmetrize = symmetrize,
    nonneg = nonneg,
    id = id
  )
}

#' @export
slather.layer_cdc_flatline_quantiles <-
  function(object, components, workflow, new_data, ...) {
    rlang::check_dots_empty()
    if (is.null(object$quantile_levels)) {
      return(components)
    }
    the_fit <- workflows::extract_fit_parsnip(workflow)
    if (!inherits(the_fit, "_flatline")) {
      cli::cli_warn(
        c(
          "Predictions for this workflow were not produced by the {.cls flatline}",
          "{.pkg parsnip} engine. Results may be unexpected. See {.fn epipredict::flatline}."
        )
      )
    }
    p <- components$predictions
    ek <- kill_time_value(key_colnames(components$mold))
    r <- grab_residuals(the_fit, components)

    avail_grps <- character(0L)
    if (length(object$by_key) > 0L) {
      cols_in_preds <- hardhat::check_column_names(p, object$by_key)
      if (!cols_in_preds$ok) {
        cli::cli_warn(c(
          "Predicted values are missing key columns: {.val {cols_in_preds$missing_names}}.",
          "Ignoring these."
        ))
      }
      if (inherits(the_fit, "_flatline")) {
        cols_in_resids <- hardhat::check_column_names(r, object$by_key)
        if (!cols_in_resids$ok) {
          cli::cli_warn(c(
            "Existing residuals are missing key columns: {.val {cols_in_resids$missing_names}}.",
            "Ignoring these."
          ))
        }
        # use only the keys that are in the predictions and requested.
        avail_grps <- intersect(ek, setdiff(
          object$by_key,
          c(cols_in_preds$missing_names, cols_in_resids$missing_names)
        ))
      } else { # not flatline, but we'll try
        key_cols <- dplyr::bind_cols(
          geo_value = components$mold$extras$roles$geo_value,
          components$mold$extras$roles$key
        )
        cols_in_resids <- hardhat::check_column_names(key_cols, object$by_key)
        if (!cols_in_resids$ok) {
          cli::cli_warn(c(
            "Requested residuals are missing key columns: {.val {cols_in_resids$missing_names}}.",
            "Ignoring these."
          ))
        }
        avail_grps <- intersect(names(key_cols), setdiff(
          object$by_key,
          c(cols_in_preds$missing_names, cols_in_resids$missing_names)
        ))
        r <- dplyr::bind_cols(key_cols, r)
      }
    }
    r <- r %>%
      dplyr::select(tidyselect::all_of(c(avail_grps, ".resid"))) %>%
      dplyr::group_by(!!!rlang::syms(avail_grps)) %>%
      dplyr::summarise(.resid = list(.resid), .groups = "drop")

    res <- dplyr::left_join(p, r, by = avail_grps) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        .pred_distn_all = propagate_samples(
          .resid, .pred, object$quantile_levels,
          object$aheads, object$nsim, object$symmetrize, object$nonneg
        )
      ) %>%
      dplyr::select(tidyselect::all_of(c(avail_grps, ".pred_distn_all")))

    # res <- check_pname(res, components$predictions, object)
    components$predictions <- dplyr::left_join(
      components$predictions,
      res,
      by = avail_grps
    )
    components
  }

propagate_samples <- function(
    r, p, quantile_levels, aheads, nsim, symmetrize, nonneg) {
  max_ahead <- max(aheads)
  if (symmetrize) {
    r <- c(r, -r)
  }
  samp <- quantile(r,
    probs = c(0, seq_len(nsim - 1)) / (nsim - 1),
    na.rm = TRUE, names = FALSE
  )
  res <- list()

  raw <- samp + p
  if (nonneg) raw <- pmax(0, raw)
  res[[1]] <- raw
  if (max_ahead > 1L) {
    for (iter in 2:max_ahead) {
      samp <- shuffle(samp)
      raw <- raw + samp
      if (symmetrize) {
        symmetric <- raw - (median(raw) - p)
      } else {
        symmetric <- raw
      }
      if (nonneg) symmetric <- pmax(0, symmetric)
      res[[iter]] <- symmetric
    }
  }
  res <- res[aheads]
  list(tibble::tibble(
    ahead = aheads,
    .pred_distn = map_vec(
      res, ~ dist_quantiles(quantile(.x, quantile_levels), quantile_levels)
    )
  ))
}

shuffle <- function(x) {
  stopifnot(is.vector(x))
  sample(x, length(x), replace = FALSE)
}
