
#' Smooth quantile regression
#'
#' @description
#' `smooth_quantile_reg()` generates a quantile regression model _specification_ for
#' the [tidymodels](https://www.tidymodels.org/) framework. Currently, the
#' only supported engine is [smoothqr::smooth_qr()].
#'
#' @param mode A single character string for the type of model.
#'   The only possible value for this model is "regression".
#' @param engine Character string naming the fitting function. Currently, only
#'   "smooth_qr" is supported.
#' @param tau A scalar or vector of values in (0, 1) to determine which
#'   quantiles to estimate (default is 0.5).
#'
#' @export
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @importFrom quantreg rq
#' @examples
#' tib <- data.frame(
#'   y1 = rnorm(100), y2 = rnorm(100), y3 = rnorm(100),
#'   y4 = rnorm(100), y5 = rnorm(100), y6 = rnorm(100),
#'   x1 = rnorm(100), x2 = rnorm(100))
#' rq_spec <- smooth_quantile_reg(tau = c(.2, .5, .8), outcome_locations = 1:6)
#' ff <- rq_spec %>% fit(cbind(y1, y2 , y3 , y4 , y5 , y6) ~ ., data = tib)
#' p <- predict(ff, new_data = tib)
smooth_quantile_reg <- function(
    mode = "regression",
    engine = "smoothqr",
    outcome_locations = NULL,
    tau = 0.5,
    degree = 3L) {

  # Check for correct mode
  if (mode != "regression") rlang::abort("`mode` must be 'regression'")
  if (engine != "smoothqr") rlang::abort("`engine` must be 'smoothqr'")

  arg_is_probabilities(tau)
  arg_is_pos_int(degree)
  arg_is_scalar(degree)
  arg_is_numeric(outcome_locations, allow_null = TRUE)
  if (is.unsorted(tau)) {
    rlang::warn("Sorting tau to increasing order.")
    tau <- sort(tau)
  }

  args <- list(tau = rlang::enquo(tau), degree = rlang::enquo(degree),
               outcome_locations = rlang::enquo(outcome_locations))

  # Save some empty slots for future parts of the specification
  parsnip::new_model_spec(
    "smooth_quantile_reg",
    args = args,
    eng_args = NULL,
    mode = mode,
    method = NULL,
    engine = engine
  )
}


make_smooth_quantile_reg <- function() {
  parsnip::set_new_model("smooth_quantile_reg")
  parsnip::set_model_mode("smooth_quantile_reg", "regression")
  parsnip::set_model_engine("smooth_quantile_reg", "regression", eng = "smoothqr")
  parsnip::set_dependency(
    "smooth_quantile_reg",
    eng = "smoothqr",
    pkg = "smoothqr",
    mode = "regression"
  )

  parsnip::set_model_arg(
    model = "smooth_quantile_reg",
    eng = "smoothqr",
    parsnip = "tau",
    original = "tau",
    func = list(pkg = "smoothqr", fun = "smooth_qr"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "smooth_quantile_reg",
    eng = "smoothqr",
    parsnip = "degree",
    original = "degree",
    func = list(pkg = "smoothqr", fun = "smooth_qr"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "smooth_quantile_reg",
    eng = "smoothqr",
    parsnip = "outcome_locations",
    original = "aheads",
    func = list(pkg = "smoothqr", fun = "smooth_qr"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "smooth_quantile_reg",
    eng = "smoothqr",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"), # prevent user from touching these
      func = c(pkg = "smoothqr", fun = "smooth_qr"),
      defaults = list(intercept = TRUE)
    )
  )

  parsnip::set_encoding(
    model = "smooth_quantile_reg",
    eng = "smoothqr",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional", # factor -> dummy conversion w/ baseline
      compute_intercept = TRUE, # put an intercept into the design matrix
      remove_intercept = TRUE, # but then remove it, we'll put it back in the function
      allow_sparse_x = FALSE # quantgen::rq can't handle sparse x, unfortunately
    )
  )

  process_smooth_qr_preds <- function(x, object) {
    object <- parsnip::extract_fit_engine(object)
    list_of_pred_distns <- lapply(x, function(p) {
      x <- unname(apply(x, 1, function(q) unname(sort(q)), simplify = FALSE))
      dist_quantiles(x, list(object$tau))
    })
    nx <- nrow(x)
    nout <- length(list_of_pred_distns)
    tib <- tibble::tibble(
      ids = rep(seq(nx), times = nout),
      ahead = rep(object$aheads, each = nx),
      distn = do.call(c, list_of_pred_distns)) %>%
      tidyr::nest(.pred = c(ahead, distn))

    return(tib[".pred"])
  }


  parsnip::set_pred(
    model = "smooth_quantile_reg",
    eng = "smoothqr",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = process_smooth_qr_preds,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )
}

