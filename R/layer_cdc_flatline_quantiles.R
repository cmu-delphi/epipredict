layer_cdc_flatline_quantiles <- function(
    frosting,
    ...,
    aheads = 1:4,
    quantiles = c(.01, .025, 1:19 / 20, .975, .99),
    nsims = 1e5,
    by_key = "geo_value",
    symmetrize = FALSE,
    nonneg = TRUE,
    id = rand_id("cdc_baseline_bands")) {

  rlang::check_dots_empty()

  arg_is_int(aheads)
  arg_is_probabilities(quantiles)
  arg_is_pos_int(nsims)
  arg_is_scalar(nsims)
  arg_is_chr_scalar(id)
  arg_is_lgl_scalar(symmetrize, nonneg)
  arg_is_chr(by_key, allow_null = TRUE, allow_na = TRUE, allow_empty = TRUE)

  add_layer(
    frosting,
    layer_cdc_flatline_quantiles_new(
      aheads = aheads,
      quantiles = quantiles,
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
    quantiles,
    nsims,
    by_key,
    symmetrize,
    nonneg,
    id
) {
  layer(
    "cdc_flatline_quantiles",
    aheads = aheads,
    quantiles = quantiles,
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
    the_fit <- workflows::extract_fit_parsnip(workflow)
    if (!inherits(the_fit, "_flatline")) {
      cli::cli_warn(
        c("Predictions for this workflow were not produced by the {.cls flatline}",
        "{.pkg parsnip} engine. Results may be unexpected. See {.fn epipredict::flatline}.")
      )
    }
    p <- components$predictions
    ek <- kill_time_value(epi_keys_mold(components$mold))
    r <- grab_residuals(the_fit, components)

    avail_grps <- character(0L)
    if (length(object$by_key) > 0L) {
      cols_in_preds <- hardhat::check_column_names(p, object$by_key)
      if (!cols_in_preds$ok) {
        cli::cli_warn(c(
          "Predicted values are missing key columns: {.var cols_in_preds$missing_names}.",
          "Ignoring these."
        ))
      }
      if (inherits(the_fit, "_flatline")) {
        cols_in_resids <- hardhat::check_column_names(r, object$by_key)
        if (!cols_in_resids$ok) {
          cli::cli_warn(c(
            "Existing residuals are missing key columns: {.var cols_in_resids$missing_names}.",
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
            "Requested residuals are missing key columns: {.var cols_in_resids$missing_names}.",
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
        .pred_distn_all = propogate_samples(
          .resid, .pred, object$quantiles,
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

propogate_samples <- function(
    r, p, quantiles, aheads, nsim, symmetrize, nonneg) {
  max_ahead <- max(aheads)
  samp <- quantile(r, probs = c(0, seq_len(nsim - 1)) / (nsim - 1), na.rm = TRUE)
  res <- list()

  # p should be all the same
  p <- max(p, na.rm = TRUE)

  raw <- samp + p
  if (nonneg) raw <- pmax(0, raw)
  res[[1]] <- raw
  if (max_ahead > 1L) {
    for (iter in 2:max_ahead) {
      samp <- shuffle(samp)
      raw <- raw + samp
      if (symmetrize) symmetric <- raw - (median(raw) - p)
      else symmetric <- raw
      if (nonneg) symmetric <- pmax(0, symmetric)
      res[[iter]] <- symmetric
    }
  }
  res <- res[aheads]
  list(tibble::tibble(
    ahead = aheads,
    .pred_distn = map_vec(
      res, ~ dist_quantiles(quantile(.x, quantiles), tau = quantiles)
    )
  ))
}

shuffle <- function(x) {
  stopifnot(is.vector(x))
  sample(x, length(x), replace = FALSE)
}
