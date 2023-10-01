#' Quantile regression
#'
#' @description
#' `quantile_reg()` generates a quantile regression model _specification_ for
#' the [tidymodels](https://www.tidymodels.org/) framework. Currently, the
#' only supported engine is "rq" which uses [quantreg::rq()].
#'
#' @param mode A single character string for the type of model.
#'   The only possible value for this model is "regression".
#' @param engine Character string naming the fitting function. Currently, only
#'   "rq" is supported.
#' @param quantile_values A scalar or vector of values in (0, 1) to determine which
#'   quantiles to estimate (default is 0.5).
#'
#' @export
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @importFrom quantreg rq
#' @examples
#' tib <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100))
#' rq_spec <- quantile_reg(quantile_values = c(.2, .8)) %>% set_engine("rq")
#' ff <- rq_spec %>% fit(y ~ ., data = tib)
#' predict(ff, new_data = tib)
quantile_reg <- function(mode = "regression", engine = "rq", quantile_values = 0.5) {
  # Check for correct mode
  if (mode != "regression") {
    cli_abort("`mode` must be 'regression'")
  }

  # Capture the arguments in quosures
  if (any(quantile_values > 1)) cli_abort("All `quantile_values` must be less than 1.")
  if (any(quantile_values < 0)) cli_abort("All `quantile_values` must be greater than 0.")
  if (is.unsorted(quantile_values)) {
    cli::cli_warn("Sorting `quantile_values` to increasing order.")
    quantile_values <- sort(quantile_values)
  }
  args <- list(quantile_values = rlang::enquo(quantile_values))

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
    parsnip = "quantile_values",
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
      protect = c("formula", "data", "weights"),
      func = c(pkg = "quantreg", fun = "rq"),
      defaults = list(
        method = "br",
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
    type <- class(object)[1]


    # can't make a method because object is second
    out <- switch(type,
      rq = dist_quantiles(unname(as.list(x)), object$quantile_values), # one quantile
      rqs = {
        x <- lapply(unname(split(x, seq(nrow(x)))), function(q) sort(q))
        dist_quantiles(x, list(object$quantile_values))
      },
      cli_abort(c(
        "Prediction is not implemented for this `rq` type.",
        i = "See {.fun quantreg::rq}."
      ))
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
