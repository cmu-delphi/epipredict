#' Returns predictive quantiles
#'
#' This function calculates quantiles when the prediction was _distributional_.
#'
#' Currently, the only distributional modes/engines are
#' * `quantile_reg()`
#' * `smooth_quantile_reg()`
#' * `rand_forest(mode = "regression") %>% set_engine("grf_quantiles")`
#'
#' If these engines were used, then this layer will grab out estimated
#' (or extrapolated) quantiles at the requested quantile values.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param quantile_levels a vector of probabilities to extract
#' @param truncate Do we truncate the distribution to an interval
#' @param name character. The name for the output column.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor. An additional column of predictive
#'   quantiles will be added to the predictions.
#' @export
#'
#' @examples
#' library(dplyr)
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, quantile_reg(quantile_levels = c(.25, .5, .75))) %>%
#'   fit(jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_quantile_distn() %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- forecast(wf1)
#' p
layer_quantile_distn <- function(frosting,
                                 ...,
                                 quantile_levels = c(.25, .75),
                                 truncate = c(-Inf, Inf),
                                 name = ".pred_distn",
                                 id = rand_id("quantile_distn")) {
  rlang::check_dots_empty()
  arg_is_chr_scalar(name, id)
  arg_is_probabilities(quantile_levels)
  stopifnot(
    length(truncate) == 2L, is.numeric(truncate), truncate[1] < truncate[2]
  )

  add_layer(
    frosting,
    layer_quantile_distn_new(
      quantile_levels = quantile_levels,
      truncate = truncate,
      name = name,
      id = id
    )
  )
}

layer_quantile_distn_new <- function(quantile_levels, truncate, name, id) {
  layer("quantile_distn",
    quantile_levels = quantile_levels,
    truncate = truncate,
    name = name,
    id = id
  )
}

#' @export
slather.layer_quantile_distn <-
  function(object, components, workflow, new_data, ...) {
    dstn <- components$predictions$.pred
    if (!inherits(dstn, "distribution")) {
      cli_abort(c(
        "`layer_quantile_distn()` requires distributional predictions.",
        "These are of class {.cls {class(dstn)}}."
      ))
    }
    rlang::check_dots_empty()

    dstn <- dist_quantiles(
      quantile(dstn, object$quantile_levels),
      object$quantile_levels
    )

    truncate <- object$truncate
    if (!all(is.infinite(truncate))) {
      dstn <- snap(dstn, truncate[1], truncate[2])
    }
    dstn <- tibble(dstn = dstn)
    dstn <- check_pname(dstn, components$predictions, object)
    components$predictions <- mutate(components$predictions, !!!dstn)
    components
  }

#' @export
print.layer_quantile_distn <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Creating predictive quantiles"
  td <- "<calculated>"
  td <- rlang::enquos(td)
  ext <- x$quantile_levels
  print_layer(td,
    title = title, width = width, conjunction = "quantile_levels",
    extra_text = ext
  )
}
