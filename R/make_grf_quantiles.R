#' Random quantile forests via grf
#'
#' [grf::quantile_forest()] fits random forests in a way that makes it easy
#' to calculate _quantile_ forests. Currently, this is the only engine
#' provided here, since quantile regression is the typical use-case.
#'
#' @section Tuning Parameters:
#'
#' This model has 3 tuning parameters:
#'
#' - `mtry`: # Randomly Selected Predictors (type: integer, default: see below)
#' - `trees`: # Trees (type: integer, default: 2000L)
#' - `min_n`: Minimal Node Size (type: integer, default: 5)
#'
#' `mtry` depends on the number of columns in the design matrix.
#' The default in [grf::quantile_forest()] is `min(ceiling(sqrt(ncol(X)) + 20), ncol(X))`.
#'
#' For categorical predictors, a one-hot encoding is always used. This makes
#' splitting efficient, but has implications for the `mtry` choice. A factor
#' with many levels will become a large number of columns in the design matrix
#' which means that some of these may be selected frequently for potential splits.
#' This is different than in other implementations of random forest. For more
#' details, see [the `grf` discussion](https://grf-labs.github.io/grf/articles/categorical_inputs.html).
#'
#' @section Translation from parsnip to the original package:
#'
#' ```{r, translate-engine}
#' rand_forest(
#'   mode = "regression", # you must specify the `mode = regression`
#'   mtry = integer(1),
#'   trees = integer(1),
#'   min_n = integer(1)
#' ) %>%
#'   set_engine("grf_quantiles") %>%
#'   translate()
#' ```
#'
#' @section Case weights:
#'
#' Case weights are not supported.
#'
#' @examples
#' library(grf)
#' tib <- data.frame(
#'   y = rnorm(100), x = rnorm(100), z = rnorm(100),
#'   f = factor(sample(letters[1:3], 100, replace = TRUE))
#' )
#' spec <- rand_forest(engine = "grf_quantiles", mode = "regression")
#' out <- fit(spec, formula = y ~ x + z, data = tib)
#' predict(out, new_data = tib[1:5, ]) %>%
#'   pivot_quantiles_wider(.pred)
#'
#' # -- adjusting the desired quantiles
#'
#' spec <- rand_forest(mode = "regression") %>%
#'   set_engine(engine = "grf_quantiles", quantiles = c(1:9 / 10))
#' out <- fit(spec, formula = y ~ x + z, data = tib)
#' predict(out, new_data = tib[1:5, ]) %>%
#'   pivot_quantiles_wider(.pred)
#'
#' # -- a more complicated task
#'
#' library(dplyr)
#' dat <- covid_case_death_rates %>%
#'   filter(time_value > as.Date("2021-10-01"))
#' rec <- epi_recipe(dat) %>%
#'   step_epi_lag(case_rate, death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#' frost <- frosting() %>%
#'   layer_predict() %>%
#'   layer_threshold(.pred)
#' spec <- rand_forest(mode = "regression") %>%
#'   set_engine(engine = "grf_quantiles", quantiles = c(.25, .5, .75))
#'
#' ewf <- epi_workflow(rec, spec, frost) %>%
#'   fit(dat) %>%
#'   forecast()
#' ewf %>%
#'   rename(forecast_date = time_value) %>%
#'   mutate(target_date = forecast_date + 7L) %>%
#'   pivot_quantiles_wider(.pred)
#'
#' @name grf_quantiles
NULL



make_grf_quantiles <- function() {
  parsnip::set_model_engine(
    model = "rand_forest", mode = "regression", eng = "grf_quantiles"
  )
  parsnip::set_dependency(
    model = "rand_forest", eng = "grf_quantiles", pkg = "grf",
    mode = "regression"
  )


  # These are the arguments to the parsnip::rand_forest() that must be
  # translated from grf::quantile_forest
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf_quantiles",
    parsnip = "mtry",
    original = "mtry",
    func = list(pkg = "dials", fun = "mtry"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf_quantiles",
    parsnip = "trees",
    original = "num.trees",
    func = list(pkg = "dials", fun = "trees"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "rand_forest",
    eng = "grf_quantiles",
    parsnip = "min_n",
    original = "min.node.size",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  # the `value` list describes how grf::quantile_forest expects to receive
  # arguments. In particular, it needs X and Y to be passed in as a matrices.
  # But the matrix interface in parsnip calls these x and y. So the data
  # slot translates them
  #
  # protect - prevents the user from passing X and Y arguments themselves
  # defaults - engine specific arguments (not model specific) that we allow
  #   the user to change
  parsnip::set_fit(
    model = "rand_forest",
    eng = "grf_quantiles",
    mode = "regression",
    value = list(
      interface = "matrix",
      protect = c("X", "Y"),
      data = c(x = "X", y = "Y"),
      func = c(pkg = "grf", fun = "quantile_forest"),
      defaults = list(
        quantiles = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
        num.threads = 1L,
        seed = rlang::expr(stats::runif(1, 0, .Machine$integer.max))
      )
    )
  )

  parsnip::set_encoding(
    model = "rand_forest",
    eng = "grf_quantiles",
    mode = "regression",
    options = list(
      # one hot is the closest to typical factor handling in randomForest
      # (1 vs all splitting), though since we aren't bagging,
      # factors with many levels could be visited frequently
      predictor_indicators = "one_hot",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # turn the predictions into a tibble with a dist_quantiles column
  process_qrf_preds <- function(x, object) {
    quantile_levels <- parsnip::extract_fit_engine(object)$quantiles.orig %>% sort()
    x <- x$predictions
    out <- lapply(vctrs::vec_chop(x), function(x) sort(drop(x)))
    out <- dist_quantiles(out, list(quantile_levels))
    return(dplyr::tibble(.pred = out))
  }

  parsnip::set_pred(
    model = "rand_forest",
    eng = "grf_quantiles",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = process_qrf_preds,
      func = c(fun = "predict"),
      # map between parsnip::predict args and grf::quantile_forest args
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        seed = rlang::expr(sample.int(10^5, 1)),
        verbose = FALSE
      )
    )
  )
}
