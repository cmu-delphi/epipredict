layer_cdc_flatline_quantiles <- function(
    frosting,
    ...,
    aheads = 1:4,
    quantiles = c(.01, .025, 1:19 / 20, .975, .99),
    nsims = 1e5,
    by_key = "geo_value",
    symmetrize = FALSE,
    id = rand_id("cdc_baseline_bands")) {

  rlang::check_dots_empty()

  arg_is_int(aheads)
  arg_is_probabilities(quantiles)
  arg_is_pos_int(nsims)
  arg_is_scalar(nsims)
  arg_is_chr_scalar(id)
  arg_is_lgl_scalar(symmetrize)
  arg_is_chr(by_key, allow_null = TRUE, allow_na = TRUE, allow_empty = TRUE)

  add_layer(
    frosting,
    layer_cdc_flatline_quantiles_new(
      aheads = aheads,
      quantiles = quantiles,
      nsims = nsims,
      by_key = by_key,
      symmetrize = symmetrize,
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
    id
) {
  layer(
    "cdc_flatline_quantiles",
    aheads,
    quantiles,
    nsims,
    by_key,
    symmetrize,
    id
  )
}

#' @export
slather.layer_cdc_flatline_quantiles <-
  function(object, components, workflow, new_data, ...) {
    the_fit <- workflows::extract_fit_parsnip(workflow)
    s <- ifelse(object$symmetrize, -1, NA)
    r <- grab_residuals(the_fit, components)

    ## Handle any grouping requests
    if (length(object$by_key) > 0L) {
      key_cols <- dplyr::bind_cols(
        geo_value = components$mold$extras$roles$geo_value,
        components$mold$extras$roles$key
      )
      common <- intersect(object$by_key, names(key_cols))
      excess <- setdiff(object$by_key, names(key_cols))
      if (length(excess) > 0L) {
        rlang::warn(
          "Requested residual grouping key(s) {excess} are unavailable ",
          "in the original data. Grouping by the remainder: {common}."
        )
      }
      if (length(common) > 0L) {
        r <- r %>% dplyr::select(tidyselect::any_of(c(common, ".resid")))
        common_in_r <- common[common %in% names(r)]
        if (length(common_in_r) != length(common)) {
          rlang::warn(
            "Some grouping keys are not in data.frame returned by the",
            "`residuals()` method. Groupings may not be correct."
          )
        }
        r <- dplyr::bind_cols(key_cols, r) %>%
          dplyr::group_by(!!!rlang::syms(common))
      }
    }






    # always return components
    components
  }

propogate_samples <- function(x, p, horizon, nsim, symmetrize) {
  samp <- quantile(x, probs = c(0, seq_len(nsim)) / nsim)

  for (iter in seq(horizon)) {}
}

