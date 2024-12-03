#' Calculate a growth rate
#'
#' `step_growth_rate()` creates a *specification* of a recipe step
#'   that will generate one or more new columns of derived data.
#'
#'
#' @inheritParams step_epi_lag
#' @param horizon Bandwidth for the sliding window, when `method` is
#'   "rel_change" or "linear_reg". See [epiprocess::growth_rate()] for more
#'   details.
#' @param method Either "rel_change" or "linear_reg",
#'   indicating the method to use for the growth rate
#'   calculation. These are local methods: they are run in a sliding
#'   fashion over the sequence (in order to estimate derivatives and hence
#'   growth rates). See [epiprocess::growth_rate()] for more details.
#' @param log_scale Should growth rates be estimated using the parameterization
#'   on the log scale? See details for an explanation. Default is `FALSE`.
#' @param replace_Inf Sometimes, the growth rate calculation can result in
#'   infinite values (if the denominator is zero, for example). In this case,
#'   most prediction methods will fail. This argument specifies potential
#'   replacement values. The default (`NA`) will likely result in these rows
#'   being removed from the data. Alternatively, you could specify arbitrary
#'   large values, or perhaps zero. Setting this argument to `NULL` will result
#'   in no replacement.
#' @param additional_gr_args_list A list of additional arguments used by
#'   [epiprocess::growth_rate()]. All `...` arguments may be passed here along
#'   with `dup_rm` and `na_rm`.
#' @template step-return
#'
#'
#' @family row operation steps
#' @importFrom epiprocess growth_rate
#' @export
#' @examples
#' r <- epi_recipe(covid_case_death_rates) %>%
#'   step_growth_rate(case_rate, death_rate)
#' r
#'
#' r %>%
#'   prep(covid_case_death_rates) %>%
#'   bake(new_data = NULL)
step_climate_predictor <-
  function(recipe,
           ...,
           role = "predictor",
           time_type = c("epiweek", "week", "month", "day"),
           center_method = c("median", "mean"),
           window_size = 3L,
           epi_keys = NULL,
           prefix = "climate_",
           skip = FALSE,
           id = rand_id("climate_predictor")) {
    if (!is_epi_recipe(recipe)) {
      cli_abort("This recipe step can only operate on an {.cls epi_recipe}.")
    }
    time_type <- rlang::arg_match(time_type)
    center_method <- rlang::arg_match(center_method)
    arg_is_chr(role)
    arg_is_chr(epi_keys, allow_null = TRUE)
    arg_is_nonneg_int(window_size)
    arg_is_scalar(window_size)
    arg_is_chr_scalar(prefix, id)
    arg_is_lgl_scalar(skip)

    aggr <- switch(time_type,
                   epiweek = lubridate::epiweek, week = lubridate::week,
                   month = lubridate::month, day = lubridate::yday)

    recipes::add_step(
      recipe,
      step_climate_predictor_new(
        terms = enquos(...),
        role = role,
        trained = FALSE,
        time_type = time_type,
        aggr = aggr,
        center_method = center_method,
        window_size = window_size,
        epi_keys = epi_keys,
        result = NULL,
        prefix = prefix,
        columns = NULL,
        skip = skip,
        id = id,
        case_weights = NULL
      )
    )
  }


step_climate_predictor_new <-
  function(terms,
           role,
           trained,
           time_type,
           aggr,
           center_method,
           window_size,
           epi_keys,
           result,
           prefix,
           columns,
           skip,
           id,
           case_weights) {
    recipes::step(
      subclass = "climate_predictor",
      terms = terms,
      role = role,
      trained = trained,
      time_type = time_type,
      aggr = aggr,
      center_method = center_method,
      window_size = window_size,
      epi_keys = epi_keys,
      result = result,
      prefix = prefix,
      columns = columns,
      skip = skip,
      id = id,
      case_weights = case_weights
    )
  }



#' @export
prep.step_climate_predictor <- function(x, training, info = NULL, ...) {
  col_names <- recipes_eval_select(x$terms, training, info)
  recipes::check_type(training[, col_names], types = c("double", "integer"))
  wts <- recipes::get_case_weights(info, training)
  were_weights_used <- recipes::are_weights_used(wts, unsupervised = TRUE)
  if (isFALSE(were_weights_used)) {
    wts <- rep(1, nrow(training))
  }

  modulus <- switch(x$time_type, epiweek = 7L, week = 7L, month = 12L, day = 365L)

  result <- training %>%
    mutate(.idx = x$aggr(time_value), .weights = wts) %>%
    select(.idx, .weights, c(col_names, x$epi_keys)) %>%
    summarize(across(
      all_of(col_names),
      ~ roll_modular_multivec(.x, .idx, .weights, x$center_method,
                              x$window_size, modulus)),
      .by = x$epi_keys
    ) %>%
    unnest(col_names)

  step_climate_predictor_new(
    terms = x$terms,
    role = x$role,
    trained = TRUE,
    time_type = x$time_type,
    aggr = x$aggr,
    center_method = x$center_method,
    window_size = x$window_size,
    epi_keys = x$epi_keys,
    result = result,
    prefix = x$prefix,
    columns = col_names,
    skip = x$skip,
    id = x$id,
    case_weights = were_weights_used
  )
}



#' @export
bake.step_climate_predictor <- function(object, new_data, ...) {
  new_data %>%
    mutate(.idx = object$aggr(.target_time_value)) %>%
    left_join(object$result, by = c(".idx", object$epi_keys)) %>%
    select(-.idx)
}


#' @export
print.step_climate_predictor <- function(x, width = max(20, options()$width - 30), ...) {
  print_epi_step(
    x$columns, x$terms, x$trained,
    title = "Calculating climate_predictor for ",
    conjunction = "by",
    extra_text = paste(x$time_type, "using the", x$center_method)
  )
  invisible(x)
}

roll_modular_multivec <- function(col, index, weights, center_method,
                                  window_size, modulus) {
  if (center_method == "mean") {
    aggr <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
  } else {
    aggr <- function(x, w) median(x, na.rm = TRUE)
  }
  tib <- tibble(col = col, weights = weights, index = index) |>
    arrange(index) |>
    tidyr::nest(data = c(col, weights), .by = index)
  out <- double(nrow(tib))
  for (iter in seq_along(out)) {
    entries <- (iter - window_size):(iter + window_size) %% modulus
    entries[entries == 0] <- modulus
    out[iter] <- with(
      purrr::list_rbind(tib$data[entries]),
      aggr(col, weights)
    )
  }
  tibble(index = unique(tib$index), climate_pred = out)
}


