make_flatline_reg <- function() {
  parsnip::set_model_engine("linear_reg", "regression", eng = "flatline")
  parsnip::set_dependency("linear_reg", eng = "flatline", pkg = "epipredict")

  flatline_reg <- function(mode = "regression") {
    if (mode  != "regression") {
      rlang::abort("`mode` should be 'regression'")
    }

    parsnip::new_model_spec(
      "flatline_reg",
      args = NULL,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

  parsnip::set_fit(
    model = "flatline_reg",
    eng = "flatline",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "epipredict", fun = "flatline"),
      defaults = list()
    ))

  parsnip::set_encoding(
    model = "flatline_reg",
    eng = "flatline",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "flatline_reg",
    eng = "flatline",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL, post = NULL, func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

}

flatline <- function(formula, data, ahead, n_recent = 1L) {
  if (!epiprocess::is_epi_df(data)) {
    rlang::abort("flatline forecaster only works for data of class `epi_df`.")
  }
  response <- recipes:::get_lhs_vars(formula, data)
  ek <- epi_keys(data)
  data <- dplyr::select(data, dplyr::all_of(c(ek, response)))
  keys <- ek[-1L]

  preds <- data %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) %>%
    dplyr::transmute(
      .pred = RcppRoll::roll_meanr(!!sym(response), n = n_recent, na.rm = TRUE),
      time_value = time_value + ahead) %>%
    dplyr::ungroup() %>%
    dplyr::right_join(data, by = ek) %>%
    dplyr::arrange(time_value) %>%
    dplyr::mutate(.resid = .pred - !!sym(response)) %>%
    dplyr::select(dplyr::all_of(c(ek, response, ".pred", ".resid"))) %>%
    copy_to_epi_df(data)

  class(preds) <- c("flatline", class(data))
  preds
}

residuals.flatline <- function(object, ...) {
  object$.resid
}

predict.flatline <- function(object, newdata, ...) {
  if (!epiprocess::is_epi_df(newdata))
    abort("`newdata` must be an epi_df to predict with the flatline forecaster.")
  if (attr(object, "metadata")$geo_type != attr(newdata, "metadata")$geo_type) {
    abort(c("`newdata` has a different geo_type than was used",
            "to fit the flatline forecaster"))
  }
  if ((attr(object, "metadata")$additional_metadata %||% TRUE) !=
      (attr(newdata, "metadata")$additional_metadata) %||% TRUE) {
    abort(c("`newdata` has different additional_metadata than was used",
            "to fit the flatline forecaster"))
  }
  keys <- epi_keys(object)[-1]
  object %>%
    dplyr::filter(time_value == max(time_value)) %>%
    dplyr::select(all_of(c(keys, ".pred"))) %>%
    dplyr::right_join(newdata, by = keys) %>%
    dplyr::pull(.pred)
}

copy_to_epi_df <- function(y, x) {
  stopifnot(epiprocess::is_epi_df(x), !epiprocess::is_epi_df(y))
  epiprocess::new_epi_df(
    y,
    attr(x, "metadata")$geo_type,
    attr(x, "metadata")$time_type,
    attr(x, "metadata")$as_of,
    attr(x, "metadata")$additional_metadata
  )
}
