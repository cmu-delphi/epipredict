#' Lower and upper thresholds for predicted values
#'
#' This post-processing step is used to set prediction values that are smaller
#' than the lower threshold or higher than the upper threshold equal to the
#' threshold values.

#' @details
#' Making case count predictions strictly positive is a typical example usage.
#'   It must be called after there is a column containing quantiles. This means at earliest it can be called after `layer_predict()` for distributional models, or after `layer_residual_quantiles()` for point prediction models. Typical best practice will use `starts_with(".pred")` as the variables to threshold.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions separated by commas. Variable names can be used as if they
#'   were positions in the data frame, so expressions like `x:y` can
#'   be used to select a range of variables. Typical usage is `.pred` to
#'   threshold predictions to a range (say, nonnegative).
#' @param lower Lower threshold for the prediction values. That is, any
#'   predictions that are less than this lower bound are set to it.
#'   Default value is `0`.
#' @param upper Upper threshold for the prediction values. That is, any
#'   predictions that are greater than this upper bound are set to it.
#'   Default value is `Inf`.
#' @param id a random id string
#'
#'
#' @return an updated `frosting` postprocessor
#' @export
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value < "2021-03-08", geo_value %in% c("ak", "ca", "ar"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#' wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_threshold(starts_with(".pred"), lower = 0.180, upper = 0.310)
#' wf <- wf %>% add_frosting(f)
#' p <- forecast(wf)
#' p
layer_threshold <-
  function(frosting, ..., lower = 0, upper = Inf, id = rand_id("threshold")) {
    arg_is_scalar(lower, upper)
    arg_is_chr_scalar(id)
    stopifnot(is.numeric(lower), is.numeric(upper), lower < upper)
    add_layer(
      frosting,
      layer_threshold_new(
        terms = enquos(...),
        lower = lower,
        upper = upper,
        id = id
      )
    )
  }


layer_threshold_new <-
  function(terms, lower, upper, id = rand_id("threshold")) {
    layer("threshold", terms = terms, lower = lower, upper = upper, id = id)
  }



#' restrict various objects to the interval \[lower, upper\]
#' @param x the object to restrict
#' @param lower numeric, the lower bound
#' @param upper numeric, the upper bound
#' @param ... unused
#' @export
#' @keywords internal
snap <- function(x, lower, upper, ...) {
  UseMethod("snap")
}

#' @export
snap.default <- function(x, lower, upper, ...) {
  rlang::check_dots_empty()
  arg_is_scalar(lower, upper)
  pmin(pmax(x, lower), upper)
}

#' @export
snap.quantile_pred <- function(x, lower, upper, ...) {
  values <- as.matrix(x)
  quantile_levels <- x %@% "quantile_levels"
  values <- map(vctrs::vec_chop(values), ~ snap(.x, lower, upper))
  quantile_pred(do.call(rbind, values), quantile_levels = quantile_levels)
}

#' @export
slather.layer_threshold <-
  function(object, components, workflow, new_data, ...) {
    rlang::check_dots_empty()
    exprs <- rlang::expr(c(!!!object$terms))
    pos <- tidyselect::eval_select(exprs, components$predictions)
    col_names <- names(pos)
    components$predictions <- components$predictions %>%
      mutate(across(all_of(col_names), ~ snap(.x, object$lower, object$upper)))
    components
  }


#' @export
print.layer_threshold <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Thresholding predictions"
  lwr <- ifelse(is.infinite(x$lower), "(", "[")
  upr <- ifelse(is.infinite(x$upper), ")", "]")
  rng <- paste0(lwr, round(x$lower, 3), ", ", round(x$upper, 3), upr)
  print_layer(x$terms,
    title = title, width = width, conjunction = "to",
    extra_text = rng
  )
}
