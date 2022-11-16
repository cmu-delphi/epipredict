
#' Quantile regression
#'
#' @description
#' `quantile_reg()` generates a quantile regression model _specification_ for
#' the [tidymodels](https://www.tidymodels.org/) framework. Currently, the
#' only supported engine is [quantreg::rq()].
#'
#' @param mode A single character string for the type of model.
#'   The only possible value for this model is "regression".
#' @param engine Character string naming the fitting function. Currently, only
#'   "rq" is supported.
#' @param tau A scalar or vector of values in (0, 1) to determine which
#'   quantiles to estimate (default is 0.5).
#'
#' @export
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @examples
#' tib <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' rq_spec <- quantile_reg(tau = c(.2, .8)) %>% set_engine("rq")
#' ff <- rq_spec %>% fit(y ~ ., data = tib)
#' predict(ff, new_data = tib)
quantile_reg <- function(mode = "regression",  engine = "rq", tau = 0.5) {
  # Check for correct mode
  if (mode  != "regression") {
    rlang::abort("`mode` should be 'regression'")
  }

  # Capture the arguments in quosures
  if (any(tau > 1)) rlang::abort("All `tau` must be less than 1.")
  if (any(tau < 0)) rlang::abort("All `tau` must be greater than 0.")
  if (is.unsorted(tau)) {
    rlang::warn("Sorting tau to increasing order.")
    tau <- sort(tau)
  }
  args <- list(tau = rlang::enquo(tau))

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
  parsnip::set_new_model("quantile_reg")
  parsnip::set_model_mode("quantile_reg", "regression")
  parsnip::set_model_engine("quantile_reg", "regression", eng = "rq")
  parsnip::set_dependency("quantile_reg", eng = "rq", pkg = "quantreg")



  parsnip::set_model_arg(
    model = "quantile_reg",
    eng = "rq",
    parsnip = "tau",
    original = "tau",
    func = list(pkg = "quantreg", fun = "rq"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "quantile_reg",
    eng = "rq",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data"),
      func = c(pkg = "quantreg", fun = "rq"),
      defaults = list(method = "br", na.action = stats::na.omit,
                      model = FALSE)
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
    type <- class(object)[1]


    # can't make a method because object is second
    out <- switch(
      type,
      rq = dist_quantiles(x, object$tau), # one quantile
      rqs = {
        x <- unname(apply(x, 1, function(q) unname(sort(q)), simplify = FALSE))
        dist_quantiles(x, list(object$tau))
      },
      rlang::abort(c("Prediction not implemented for this `rq` type.",
                     i = "See `?quantreg::rq`."))
    )
    return(data.frame(.pred = out))
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
