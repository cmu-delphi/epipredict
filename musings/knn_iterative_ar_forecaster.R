#' KNN enhanced iterative AR forecaster with optional covariates
#'
#' @param x Unused covariates. Must to be missing (resulting in AR on `y`) .
#' @param y Response.
#' @param key_vars Factor(s). A prediction will be made for each unique
#'   combination.
#' @param time_value the time value associated with each row of measurements.
#' @param args Additional arguments specifying the forecasting task. Created
#'   by calling `knn_iteraive_ar_args_list()`.
#'
#' @return A data frame of point (and optionally interval) forecasts at multiple
#'   aheads (multiple horizons from one to specified `ahead`) for each unique combination of `key_vars`.
#' @export

knn_iteraive_ar_forecaster <- function(x, y, key_vars, time_value,
                                       args = knn_iteraive_ar_args_list()) {

  # TODO: function to verify standard forecaster signature inputs
  assign_arg_list(args)
  if (is.null(key_vars)) { # this is annoying/repetitive, seemingly necessary?
    keys <- NULL
    distinct_keys <- tibble(.dump = NA)
  } else {
    keys <- tibble::tibble(key_vars)
    distinct_keys <- dplyr::distinct(keys)
  }
  if (!is.null(x)) warning("The current version for KNN enhanced iterative forecasting strategy does not support covariates. 'x' will not be used!")


  # generate data
  pool <- create_lags_and_leads(NULL, y, c(1:query_window_len), 1:ahead, time_value, keys)
  # Return NA if insufficient training data
  if (nrow(pool) < topK) {
    qnames <- probs_to_string(levels)
    out <- dplyr::bind_cols(distinct_keys, point = NA) %>%
      dplyr::select(!dplyr::any_of(".dump"))
    return(enframer(out, qnames))
  }
  # get test data
  time_keys <- data.frame(keys, time_value)
  test_time_value <- max(time_value)
  common_names <- names(time_keys)
  key_names <- setdiff(common_names, "time_value")
  Querys <- dplyr::left_join(time_keys, pool, by = common_names) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_names))) %>%
    tidyr::fill(dplyr::starts_with("x")) %>%
    dplyr::filter(time_value == test_time_value) %>%
    select(!dplyr::starts_with("y")) %>%
    drop_na()

  # embed querys and pool
  pool_raw <- pool
  pool <- pool %>%
    select(common_names, dplyr::starts_with("x"), "y1") %>%
    drop_na()
  pool_idx <- pool[common_names]

  Querys_idx <- Querys[common_names]
  pool_emb <- embedding(pool %>% select(-common_names, -dplyr::starts_with("y")))
  # iterative prediction procedure

  tmp <- data.frame()
  for (i in 1:nrow(Querys)) {
    query <- as.numeric(Querys[i, -c(1:2)])

    for (h in 1:ahead) {
      if (h == 1 | update_model) {
        query_emb <- embedding(t(query))[1, ]
        sims <- pool_emb %*% query_emb
        topk_id <- tensr:::topK(sims, topK)
        train_id <- pool_idx[topk_id, ]
        train_da <- train_id %>%
          left_join(pool, by = common_names) %>%
          select("y1", paste("x", lags, sep = ""))

        if (intercept) train_da$x0 <- 1
        obj <- stats::lm(
          y1 ~ . + 0,
          data = train_da
        )
      }

      test_da <- data.frame(t(query[lags]))
      names(test_da) <- paste("x", lags, sep = "")
      if (intercept) test_da$x0 <- 1
      point <- stats::predict(obj, test_da)

      yname <- paste("y", h, sep = "")
      residual_pool <- pool_raw %>%
        select(common_names, dplyr::starts_with("x"), yname) %>%
        drop_na()
      residual_pool_emb <- embedding(residual_pool %>% select(-common_names, -yname))
      sims <- residual_pool_emb %*% query_emb
      topk_id <- tensr:::topK(sims, topK)

      residual_da <- residual_pool[topk_id, ]
      gty <- residual_da[yname]
      residual_da <- residual_pool[topk_id, ] %>%
        select(-common_names, -yname) %>%
        as.matrix()
      for (j in 1:h) {
        residual_tmp <- data.frame(residual_da[, lags])
        names(residual_tmp) <- paste("x", lags, sep = "")
        if (intercept) residual_tmp$x0 <- 1
        pred <- stats::predict(obj, residual_tmp)
        residual_da <- cbind(pred, residual_da[, -query_window_len])
      }

      r <- (gty - pred)[, 1] / pred
      r[is.na(r)] <- 0
      q <- residual_quantiles_normlized(r, point, levels, symmetrize)
      q <- cbind(Querys_idx[i, key_names], q)
      q$ahead <- h
      tmp <- bind_rows(tmp, q)

      query <- c(point, query[-query_window_len])
    }
  }
  if (nonneg) {
    tmp <- dplyr::mutate(tmp, dplyr::across(!ahead, ~ pmax(.x, 0)))
  }

  res <- tmp %>%
    dplyr::select(!dplyr::any_of(".dump")) %>%
    dplyr::relocate(ahead)
  return(res)
}



#'KNN enhanced iterative AR forecaster argument constructor
#'
#' Constructs a list of arguments for [knn_iteraive_ar_forecaster()].
#'
#' @template param-lags
#' @template param-query_window_len
#' @template param-topK
#' @template param-ahead
#' @template param-min_train_window
#' @template param-levels
#' @template param-intercept
#' @template param-symmetrize
#' @template param-nonneg
#' @template param-update_model
#' @param quantile_by_key Not currently implemented
#'
#' @return A list containing updated parameter choices.
#' @export
#'
#' @examples
#' arx_args_list()
#' arx_args_list(symmetrize = FALSE)
#' arx_args_list(levels = c(.1, .3, .7, .9), min_train_window = 120)
knn_iteraive_ar_args_list <- function(lags = c(0, 7, 14),
                                      query_window_len = 50,
                                      topK = 500,
                                      ahead = 7,
                                      min_train_window = 20,
                                      levels = c(0.05, 0.95),
                                      intercept = TRUE,
                                      symmetrize = TRUE,
                                      nonneg = TRUE,
                                      quantile_by_key = FALSE,
                                      update_model = TRUE) {

  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  arg_is_scalar(ahead, min_train_window, query_window_len, topK)
  arg_is_nonneg_int(ahead, min_train_window, lags, query_window_len, topK)
  arg_is_lgl(intercept, symmetrize, nonneg, update_model)
  arg_is_probabilities(levels, allow_null = TRUE)

  max_lags <- max(lags)

  list(
    lags = .lags, ahead = as.integer(ahead),
    query_window_len = query_window_len,
    topK = topK,
    min_train_window = min_train_window,
    levels = levels, intercept = intercept,
    symmetrize = symmetrize, nonneg = nonneg,
    max_lags = max_lags,
    update_model = update_model
  )
}
