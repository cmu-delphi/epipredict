#' Quantile regression
#'
#' @description
#' `quantile_reg()` generates a quantile regression model _specification_ for
#' the [tidymodels](https://www.tidymodels.org/) framework. Currently, the
#' only supported engines are "rq", which uses [quantreg::rq()].
#' Quantile regression is also possible by combining [parsnip::rand_forest()]
#' with the `grf` engine. See [grf_quantiles].
#'
#' @param mode A single character string for the type of model.
#'   The only possible value for this model is "regression".
#' @param engine Character string naming the fitting function. Currently, only
#'   "rq" and "grf" are supported.
#' @param quantile_levels A scalar or vector of values in (0, 1) to determine which
#'   quantiles to estimate (default is the set 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95).
#' @param method A fitting method used by [quantreg::rq()]. See the
#'   documentation for a list of options.
#'
#' @export
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#'
#' @examples
#' library(quantreg)
#' tib <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' rq_spec <- quantile_reg(quantile_levels = c(.2, .8)) %>% set_engine("rq")
#' ff <- rq_spec %>% fit(y ~ ., data = tib)
#' predict(ff, new_data = tib)
quantile_reg <- function(mode = "regression", engine = "rq",
                         quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
                         method = "br") {
  # Check for correct mode
  if (mode != "regression") {
    cli_abort("`mode` must be 'regression'")
  }

  # Capture the arguments in quosures
  if (any(quantile_levels > 1)) cli_abort("All `quantile_levels` must be less than 1.")
  if (any(quantile_levels < 0)) cli_abort("All `quantile_levels` must be greater than 0.")
  if (is.unsorted(quantile_levels)) {
    cli_warn("Sorting `quantile_levels` to increasing order.")
    quantile_levels <- sort(quantile_levels)
  }
  args <- list(quantile_levels = rlang::enquo(quantile_levels), method = rlang::enquo(method))

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    "quantile_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}

make_quantile_reg <- function() {
  model_env <- get_model_env()
  if (!("quantile_reg" %in% model_env$models)) {
    parsnip::set_new_model("quantile_reg")
  }
  parsnip::set_model_mode("quantile_reg", "regression")
  parsnip::set_model_engine("quantile_reg", "regression", eng = "rq")
  parsnip::set_dependency("quantile_reg", eng = "rq", pkg = "quantreg")

  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "rq",
    parsnip = "quantile_levels",
    original = "tau",
    func = list(pkg = "quantreg", fun = "rq"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "rq",
    parsnip = "method",
    original = "method",
    func = list(pkg = "quantreg", fun = "rq"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "quantile_reg",
    eng = "rq",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "quantreg", fun = "rq"),
      defaults = list(
        na.action = rlang::expr(stats::na.omit),
        model = FALSE
      )
    )
  )

  parsnip::set_encoding(
    model = "quantile_reg",
    eng = "rq",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  process_rq_preds <- function(x, object) {
    object <- parsnip::extract_fit_engine(object)
    if (!is.matrix(x)) x <- as.matrix(x)
    rownames(x) <- NULL
    n_pred_quantiles <- ncol(x)
    quantile_levels <- object$tau
    tibble(.pred = hardhat::quantile_pred(x, quantile_levels))
  }

  parsnip::set_pred(
    model = "quantile_reg",
    eng = "rq",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = process_rq_preds,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )
}
