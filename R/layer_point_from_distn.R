#' Converts distributional forecasts to point forecasts
#'
#' This function adds a postprocessing layer to extract a point forecast from
#' a distributional forecast. NOTE: With default arguments, this will remove
#' information, so one should usually call this AFTER `layer_quantile_distn()`
#' or set the `name` argument to something specific.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param type character. Either `mean` or `median`.
#' @param name character. The name for the output column. The default `NULL`
#'   will overwrite the `.pred` column, removing the distribution information.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor.
#' @export
#'
#' @examples
#' library(dplyr)
#' jhu <- case_death_rate_subset %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, quantile_reg(quantile_levels = c(.25, .5, .75))) %>%
#'   fit(jhu)
#'
#' f1 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_quantile_distn() %>% # puts the other quantiles in a different col
#'   layer_point_from_distn() %>% # mutate `.pred` to contain only a point prediction
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f1)
#'
#' p1 <- forecast(wf1)
#' p1
#'
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_point_from_distn() %>% # mutate `.pred` to contain only a point prediction
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- forecast(wf2)
#' p2
layer_point_from_distn <- function(frosting,
                                   ...,
                                   type = c("median", "mean"),
                                   name = NULL,
                                   id = rand_id("point_from_distn")) {
  rlang::check_dots_empty()
  arg_is_chr_scalar(id)
  arg_is_chr_scalar(name, allow_null = TRUE)
  arg_is_chr(type)
  type <- match.arg(type)

  add_layer(
    frosting,
    layer_point_from_distn_new(
      type = type,
      name = name,
      id = id
    )
  )
}

layer_point_from_distn_new <- function(type, name, id) {
  layer("point_from_distn",
    type = type,
    name = name,
    id = id
  )
}

#' @export
slather.layer_point_from_distn <-
  function(object, components, workflow, new_data, ...) {
    dstn <- components$predictions$.pred
    if (!inherits(dstn, "distribution")) {
      rlang::warn(
        c("`layer_point_from_distn` requires distributional predictions.",
          i = "These are of class {class(dstn)}. Ignoring this layer."
        )
      )
      return(components)
    }
    rlang::check_dots_empty()

    dstn <- match.fun(object$type)(dstn)
    if (is.null(object$name)) {
      components$predictions$.pred <- dstn
    } else {
      dstn <- tibble(dstn = dstn)
      dstn <- check_pname(dstn, components$predictions, object)
      components$predictions <- mutate(components$predictions, !!!dstn)
    }
    components
  }

#' @export
print.layer_point_from_distn <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Extracting point predictions"
  if (is.null(x$name)) {
    cnj <- NULL
    ext <- "<overwriting .pred>"
  } else {
    cnj <- "adding column"
    ext <- x$name
  }
  print_layer(title = title, width = width, conjunction = cnj, extra_text = ext)
}
