make_flatline_reg <- function() {
  parsnip::set_model_engine("linear_reg", "regression", eng = "flatline")
  parsnip::set_dependency("linear_reg", eng = "flatline", pkg = "epipredict")

  parsnip::set_fit(
    model = "linear_reg",
    eng = "flatline",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "epipredict", fun = "flatline"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "linear_reg",
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
    model = "linear_reg",
    eng = "flatline",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL, post = NULL, func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )
}
