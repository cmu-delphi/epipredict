#' Smooth quantile regression
#'
#' @description
#' `smooth_quantile_reg()` generates a quantile regression model _specification_ for
#' the [tidymodels](https://www.tidymodels.org/) framework. Currently, the
#' only supported engine is [smoothqr::smooth_qr()].
#'
#' @inheritParams quantile_reg
#' @param outcome_locations Defaults to the vector `1:ncol(y)` but if the
#'   responses are observed at a different spacing (or appear in a different
#'   order), that information should be used here. This
#'   argument will be mapped to the `ahead` argument of [smoothqr::smooth_qr()].
#' @param degree the number of polynomials used for response smoothing. Must
#'   be no more than the number of responses.
#' @export
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @examples
#' library(smoothqr)
#' tib <- data.frame(
#'   y1 = rnorm(100), y2 = rnorm(100), y3 = rnorm(100),
#'   y4 = rnorm(100), y5 = rnorm(100), y6 = rnorm(100),
#'   x1 = rnorm(100), x2 = rnorm(100)
#' )
#' qr_spec <- smooth_quantile_reg(quantile_levels = c(.2, .5, .8), outcome_locations = 1:6)
#' ff <- qr_spec %>% fit(cbind(y1, y2, y3, y4, y5, y6) ~ ., data = tib)
#' p <- predict(ff, new_data = tib)
#'
#' x <- -99:99 / 100 * 2 * pi
#' y <- sin(x) + rnorm(length(x), sd = .1)
#' fd <- x[length(x) - 20]
#' XY <- smoothqr::lagmat(y[1:(length(y) - 20)], c(-20:20))
#' XY <- as_tibble(XY)
#' qr_spec <- smooth_quantile_reg(quantile_levels = c(.2, .5, .8), outcome_locations = 20:1)
#' tt <- qr_spec %>% fit_xy(x = XY[, 21:41], y = XY[, 1:20])
#'
#' pl <- predict(
#'   object = tt,
#'   new_data = XY[max(which(complete.cases(XY[, 21:41]))), 21:41]
#' )
#' pl <- pl %>%
#'   unnest(.pred) %>%
#'   mutate(distn = nested_quantiles(distn)) %>%
#'   unnest(distn) %>%
#'   mutate(
#'     x = x[length(x) - 20] + ahead / 100 * 2 * pi,
#'     ahead = NULL
#'   ) %>%
#'   pivot_wider(names_from = quantile_levels, values_from = values)
#' plot(x, y, pch = 16, xlim = c(pi, 2 * pi), col = "lightgrey")
#' curve(sin(x), add = TRUE)
#' abline(v = fd, lty = 2)
#' lines(pl$x, pl$`0.2`, col = "blue")
#' lines(pl$x, pl$`0.8`, col = "blue")
#' lines(pl$x, pl$`0.5`, col = "red")
#'
#' library(ggplot2)
#' ggplot(data.frame(x = x, y = y), aes(x)) +
#'   geom_ribbon(data = pl, aes(ymin = `0.2`, ymax = `0.8`), fill = "lightblue") +
#'   geom_point(aes(y = y), colour = "grey") + # observed data
#'   geom_function(fun = sin, colour = "black") + # truth
#'   geom_vline(xintercept = fd, linetype = "dashed") + # end of training data
#'   geom_line(data = pl, aes(y = `0.5`), colour = "red") + # median prediction
#'   theme_bw() +
#'   coord_cartesian(xlim = c(0, NA)) +
#'   ylab("y")
smooth_quantile_reg <- function(
    mode = "regression",
    engine = "smoothqr",
    outcome_locations = NULL,
    quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
    degree = 3L) {
  # Check for correct mode
  if (mode != "regression") cli_abort("`mode` must be 'regression'")
  if (engine != "smoothqr") cli_abort("`engine` must be 'smoothqr'")

  arg_is_probabilities(quantile_levels)
  arg_is_pos_int(degree)
  arg_is_scalar(degree)
  arg_is_numeric(outcome_locations, allow_null = TRUE)
  if (is.unsorted(quantile_levels)) {
    rlang::warn("Sorting `quantile_levels` to increasing order.")
    quantile_levels <- sort(quantile_levels)
  }

  args <- list(
    quantile_levels = rlang::enquo(quantile_levels),
    degree = rlang::enquo(degree),
    outcome_locations = rlang::enquo(outcome_locations)
  )

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
  model_env <- get_model_env()
  if (!("smooth_quantile_reg" %in% model_env$models)) {
    parsnip::set_new_model("smooth_quantile_reg")
  }
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
    parsnip = "quantile_levels",
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
      x <- lapply(unname(split(
        p, seq(nrow(p))
      )), function(q) unname(sort(q, na.last = TRUE)))
      dist_quantiles(x, list(object$tau))
    })
    n_preds <- length(list_of_pred_distns[[1]])
    nout <- length(list_of_pred_distns)
    tib <- tibble::tibble(
      ids = rep(seq(n_preds), times = nout),
      ahead = rep(object$aheads, each = n_preds),
      distn = do.call(c, unname(list_of_pred_distns))
    ) %>%
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
