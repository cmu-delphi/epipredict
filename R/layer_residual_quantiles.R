#' Creates predictions based on residual quantiles
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param quantile_levels numeric vector of probabilities with values in (0,1)
#'   referring to the desired quantile.
#' @param symmetrize logical. If `TRUE` then interval will be symmetric.
#' @param by_key A character vector of keys to group the residuals by before
#'   calculating quantiles. The default, `c()` performs no grouping.
#' @param name character. The name for the output column.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with additional columns of the
#'   residual quantiles added to the prediction
#' @export
#' @examples
#' jhu <- case_death_rate_subset %>%
#'   dplyr::filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>% fit(jhu)
#'
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles(quantile_levels = c(0.0275, 0.975), symmetrize = FALSE) %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#'
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles(quantile_levels = c(0.3, 0.7), by_key = "geo_value") %>%
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- predict(wf2, latest)
layer_residual_quantiles <- function(
    frosting, ...,
    quantile_levels = c(0.05, 0.95),
    symmetrize = TRUE,
    by_key = character(0L),
    name = ".pred_distn",
    id = rand_id("residual_quantiles")) {
  rlang::check_dots_empty()
  arg_is_scalar(symmetrize)
  arg_is_chr_scalar(name, id)
  arg_is_chr(by_key, allow_empty = TRUE)
  arg_is_probabilities(quantile_levels)
  arg_is_lgl(symmetrize)
  add_layer(
    frosting,
    layer_residual_quantiles_new(
      quantile_levels = quantile_levels,
      symmetrize = symmetrize,
      by_key = by_key,
      name = name,
      id = id
    )
  )
}

layer_residual_quantiles_new <- function(
    quantile_levels, symmetrize, by_key, name, id) {
  layer("residual_quantiles",
    quantile_levels = quantile_levels, symmetrize = symmetrize,
    by_key = by_key, name = name, id = id
  )
}

#' @export
slather.layer_residual_quantiles <-
  function(object, components, workflow, new_data, ...) {
    the_fit <- workflows::extract_fit_parsnip(workflow)

    if (is.null(object$quantile_levels)) {
      return(components)
    }

    s <- ifelse(object$symmetrize, -1, NA)
    r <- grab_residuals(the_fit, components)

    ## Handle any grouping requests
    if (length(object$by_key) > 0L) {
      key_cols <- dplyr::bind_cols(
        geo_value = components$mold$extras$roles$geo_value,
        components$mold$extras$roles$key
      )
      common <- intersect(object$by_key, names(key_cols))
      excess <- setdiff(object$by_key, names(key_cols))
      if (length(excess) > 0L) {
        cli::cli_warn(c(
          "Requested residual grouping key(s) {.val {excess}} are unavailable ",
          "in the original data. Grouping by the remainder: {.val {common}}."
        ))
      }
      if (length(common) > 0L) {
        r <- r %>% dplyr::select(tidyselect::any_of(c(common, ".resid")))
        common_in_r <- common[common %in% names(r)]
        if (length(common_in_r) != length(common)) {
          cli::cli_warn(c(
            "Some grouping keys are not in data.frame returned by the",
            "`residuals()` method. Groupings may not be correct."
          ))
        }
        r <- dplyr::bind_cols(key_cols, r) %>%
          dplyr::group_by(!!!rlang::syms(common))
      }
    }

    r <- r %>%
      dplyr::summarize(
        dstn = list(quantile(
          c(.resid, s * .resid),
          probs = object$quantile_levels, na.rm = TRUE
        ))
      )

    estimate <- components$predictions$.pred
    res <- tibble::tibble(
      .pred_distn = dist_quantiles(map2(estimate, r$dstn, "+"), object$quantile_levels)
    )
    res <- check_pname(res, components$predictions, object)
    components$predictions <- dplyr::mutate(components$predictions, !!!res)
    components
  }

grab_residuals <- function(the_fit, components) {
  if (the_fit$spec$mode != "regression") {
    cli::cli_abort("For meaningful residuals, the predictor should be a regression model.")
  }
  r <- stats::residuals(the_fit$fit)
  if (!is.null(r)) { # Got something from the method
    if (inherits(r, "data.frame")) {
      if (".resid" %in% names(r)) { # success
        return(r)
      } else { # failure
        cli::cli_warn(c(
          "The `residuals()` method for objects of class {.cls {cl}} results in",
          "a data frame without a column named `.resid`.",
          i = "Residual quantiles will be calculated directly from the",
          i = "difference between predictions and observations.",
          i = "This may result in unexpected behaviour."
        ))
      }
    } else if (is.vector(drop(r))) { # also success
      return(tibble(.resid = drop(r)))
    } else { # failure
      cli::cli_warn(c(
        "The `residuals()` method for objects of class {.cls {cl}} results in an",
        "object that is neither a data frame with a column named `.resid`,",
        "nor something coercible to a vector.",
        i = "Residual quantiles will be calculated directly from the",
        i = "difference between predictions and observations.",
        i = "This may result in unexpected behaviour."
      ))
    }
  }
  # The method failed for one reason or another and a warning was issued
  # Or there was no method available.
  yhat <- predict(the_fit, new_data = components$mold$predictors)
  r <- c(components$mold$outcomes - yhat)[[1]] # this will be a vector
  r <- tibble(.resid = r)
  r
}

#' @export
print.layer_residual_quantiles <- function(
    x, width = max(20, options()$width - 30), ...) {
  title <- "Resampling residuals for predictive quantiles"
  td <- "<calculated>"
  td <- rlang::enquos(td)
  ext <- x$quantile_levels
  print_layer(td,
    title = title, width = width, conjunction = "quantile_levels",
    extra_text = ext
  )
}
