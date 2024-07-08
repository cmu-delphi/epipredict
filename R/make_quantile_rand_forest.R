
make_quantile_random_forest <- function() {
  model_env <- get_model_env()
  if (!("quantile_reg" %in% model_env$models)) {
    parsnip::set_new_model("quantile_reg")
    parsnip::set_model_mode("quantile_reg", "regression")
  }
  parsnip::set_model_engine("quantile_reg", "regression", "grf")
  parsnip::set_dependency("quantile_reg", "grf", "grf", mode = "regression")



  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "grf",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "grf",
    parsnip = "trees",
    original = "num.trees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "grf",
    parsnip = "min_n",
    original = "min.node.size",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "grf",
    parsnip = "quantile_levels",
    original = "quantiles",
    func = list(pkg = "grf", fun = "quantile_forest"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "grf",
    parsnip = "x",
    original = "X",
    func = list(pkg = "grf", fun = "quantile_forest"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "grf",
    parsnip = "y",
    original = "Y",
    func = list(pkg = "grf", fun = "quantile_forest"),
    has_submodel = FALSE
  )
  parsnip::set_fit(
    model = "quantile_reg",
    eng = "grf",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("X", "Y"),
      data = c(x = "X", y = "Y"),
      func = c(pkg = "grf", fun = "quantile_forest"),
      defaults = list(
        num.threads = 1,
        seed = expr(runif(1, 0, .Machine$integer.max))
      )
    )
  )

  parsnip::set_encoding(
    model = "quantile_reg",
    eng = "grf",
    mode = "regression",
    options = list(
      predictor_indicators = "one_hot",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  process_qrf_preds <- function(x, object) {
    quantile_levels <- parsnip::extract_fit_engine(object)$quantiles.orig
    x <- x$predictions
    out <- lapply(vctrs::vec_chop(x), function(x) sort(drop(x)))
    out <- dist_quantiles(out, list(quantile_levels))
    return(dplyr::tibble(.pred = out))
  }

  parsnip::set_pred(
    model = "quantile_reg",
    eng = "grf",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = process_qrf_preds,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        seed = expr(sample.int(10 ^ 5, 1)),
        verbose = FALSE
      )
    )
  )


}
