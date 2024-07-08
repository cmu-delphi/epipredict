make_quantile_random_forest <- function() {
  parsnip::set_model_engine("rand_forest", "regression", "grf")
  parsnip::set_dependency("rand_forest", "grf", "grf", mode = "regression")


  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf",
    parsnip = "trees",
    original = "num.trees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf",
    parsnip = "min_n",
    original = "min.node.size",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "rand_forest",
    eng = "grf",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("X", "Y"),
      data = c(x = "X", y = "Y"),
      func = c(pkg = "grf", fun = "quantile_forest"),
      defaults = list(
        quantiles = c(0.1, 0.5, 0.9),
        num.threads = 1L,
        seed = expr(runif(1, 0, .Machine$integer.max))
      )
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
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
    model = "rand_forest",
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
