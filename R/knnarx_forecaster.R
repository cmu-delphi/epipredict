#' KNN enhanced ARX forecaster with optional covariates
#'
#' @param x Covariates. Allowed to be missing (resulting in AR on `y`).
#' @param y Response.
#' @param key_vars Factor(s). A prediction will be made for each unique
#'   combination.
#' @param time_value the time value associated with each row of measurements.
#' @param args Additional arguments specifying the forecasting task. Created
#'   by calling `knnarx_args_list()`.
#'
#' @return A data frame of point (and optionally interval) forecasts at a single
#'   ahead (unique horizon) for each unique combination of `key_vars`.
#' @export

knnarx_forecaster <- function(x, y, key_vars, time_value,
                              args = knnarx_args_list()) {

  # TODO: function to verify standard forecaster signature inputs
  assign_arg_list(args)
  if (is.null(key_vars)) { # this is annoying/repetitive, seemingly necessary?
    keys <- NULL
    distinct_keys <- tibble(.dump = NA)
  } else {
    keys <- tibble::tibble(key_vars)
    distinct_keys <- dplyr::distinct(keys)
  }



  # generate data
  dat <- create_lags_and_leads(x, y, lags, ahead, time_value, keys)
  if (intercept) dat$x0 <- 1
  pool <- create_lags_and_leads(NULL, y, c(1:query_window_len), ahead, time_value, keys)

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
  PredData <- dplyr::left_join(time_keys, dat, by = common_names) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_names))) %>%
    tidyr::fill(dplyr::starts_with("x")) %>%
    dplyr::filter(time_value == test_time_value)

  Querys <- dplyr::left_join(time_keys, pool, by = common_names) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(key_names))) %>%
    tidyr::fill(dplyr::starts_with("x")) %>%
    dplyr::filter(time_value == test_time_value) %>%
    select(!dplyr::starts_with("y")) %>%
    drop_na()

  # clean training data and pools
  idxs <- dplyr::inner_join(pool %>% drop_na() %>% select(common_names),
    dat %>% drop_na() %>% select(common_names),
    by = common_names
  )
  pool <- dplyr::inner_join(idxs,
    pool,
    by = common_names
  ) %>%
    select(!dplyr::starts_with("y"))

  dat <- dplyr::inner_join(idxs,
    dat,
    by = common_names
  )

  # embed querys and pool
  pool_idx <- pool[common_names]
  Querys_idx <- Querys[common_names]

  pool <- embedding(pool[, 3:ncol(pool)])
  Querys <- embedding(Querys[, 3:ncol(Querys)])
  sims <- Querys %*% t(pool)

  tmp <- data.frame()
  for (i in 1:nrow(sims)) {
    topk_id <- tensr:::topK(sims[i, ], topK)
    train_id <- pool_idx[topk_id, ]
    train_da <- train_id %>% left_join(dat, by = common_names)
    obj <- stats::lm(
      y1 ~ . + 0,
      data = train_da %>% dplyr::select(starts_with(c("x", "y")))
    )

    point <- stats::predict(obj, Querys_idx[i,] %>% left_join(PredData, by = common_names))
    r <- residuals(obj)
    q <- residual_quantiles(r, point, levels, symmetrize)
    tmp <- rbind(tmp, q)
  }

  if (nonneg) {
    tmp <- dplyr::mutate(tmp, dplyr::across(dplyr::everything(), ~ pmax(.x, 0)))
  }
  return(
    dplyr::bind_cols(Querys_idx[key_names], tmp) %>%
      dplyr::select(!dplyr::any_of(".dump"))
  )
}



#'KNN enhanced ARX forecaster argument constructor
#'
#' Constructs a list of arguments for [knnarx_forecaster()].
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
#' @param quantile_by_key Not currently implemented
#'
#' @return A list containing updated parameter choices.
#' @export
#'
#' @examples
#' knnarx_args_list()
#' knnarx_args_list(symmetrize = FALSE)
#' knnarx_args_list(levels = c(.1, .3, .7, .9), min_train_window = 120)
knnarx_args_list <- function(lags = c(0, 7, 14),
                             query_window_len = 50,
                             topK = 500,
                             ahead = 7,
                             min_train_window = 20,
                             levels = c(0.05, 0.95), intercept = TRUE,
                             symmetrize = TRUE,
                             nonneg = TRUE,
                             quantile_by_key = FALSE) {

  # error checking if lags is a list
  .lags <- lags
  if (is.list(lags)) lags <- unlist(lags)

  arg_is_scalar(ahead, min_train_window,query_window_len,topK)
  arg_is_nonneg_int(ahead, min_train_window, lags,query_window_len,topK)
  arg_is_lgl(intercept, symmetrize, nonneg)
  arg_is_probabilities(levels, allow_null = TRUE)

  max_lags <- max(lags)

  list(
    lags = .lags, ahead = as.integer(ahead),
    query_window_len = query_window_len,
    topK = topK,
    min_train_window = min_train_window,
    levels = levels, intercept = intercept,
    symmetrize = symmetrize, nonneg = nonneg,
    max_lags = max_lags
  )
}
