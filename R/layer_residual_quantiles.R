#' Creates predictions based on residual quantiles
#'
#' This function calculates predictive quantiles based on the empirical
#' quantiles of the model's residuals. If the model producing the forecast is
#' distributional, it is recommended to use `layer_residual_quantiles()`
#' instead, as those will be more accurate.
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param quantile_levels numeric vector of probabilities with values in (0,1)
#'   referring to the desired quantile. Note that 0.5 will always be included
#'   even if left out by the user.
#' @param symmetrize logical. If `TRUE` then the interval will be symmetric.
#'   Typically, one would only want non-symmetric quantiles when increasing
#'   trajectories are quite different from decreasing ones, such as a strictly
#'   postive variable near zero.
#' @param by_key A character vector of keys to group the residuals by before
#'   calculating quantiles. The default, `c()` performs no grouping.
#' @param name character. The name for the output column.
#' @param id a random id string
#'
#' @return an updated `frosting` postprocessor with additional columns of the
#'   residual quantiles added to the prediction
#' @export
#' @examples
#' jhu <- covid_case_death_rates %>%
#'   filter(time_value > "2021-11-01", geo_value %in% c("ak", "ca", "ny"))
#'
#' r <- epi_recipe(jhu) %>%
#'   step_epi_lag(death_rate, lag = c(0, 7, 14)) %>%
#'   step_epi_ahead(death_rate, ahead = 7) %>%
#'   step_epi_naomit()
#'
#' wf <- epi_workflow(r, linear_reg()) %>% fit(jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles(
#'     quantile_levels = c(0.025, 0.975),
#'     symmetrize = FALSE
#'   ) %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- forecast(wf1)
#'
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles(
#'     quantile_levels = c(0.3, 0.7),
#'     by_key = "geo_value"
#'   ) %>%
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- forecast(wf2)
layer_residual_quantiles <- function(
    frosting, ...,
    quantile_levels = c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95),
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
  quantile_levels <- sort(unique(c(0.5, quantile_levels)))
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
    rlang::check_dots_empty()

    the_fit <- workflows::extract_fit_parsnip(workflow)

    if (is.null(object$quantile_levels)) {
      return(components)
    }

    symmetric <- ifelse(object$symmetrize, -1, NA)
    r <- grab_residuals(the_fit, components)

    ## Handle any grouping requests
    common <- character(0L)
    if (length(object$by_key) > 0L) {
      key_cols <- bind_cols(
        geo_value = components$mold$extras$roles$geo_value,
        components$mold$extras$roles$key
      )
      common <- intersect(object$by_key, names(key_cols))
      excess <- setdiff(object$by_key, names(key_cols))
      if (length(excess) > 0L) {
        cli_warn(c(
          "Residual grouping key{?s} {.val {excess}} {?was/were} not found in the training data.",
          i = if (length(common) > 0L) {
            "Residual quantiles will be grouped by the remaining key{?s}: {.val {common}}."
          } else {
            "Residual quantiles will be calculated across all observations without grouping."
          }
        ))
      }
      if (length(common) > 0L) {
        r <- r %>% select(any_of(c(common, ".resid")))
        if (!all(common %in% names(r))) {
          cli_warn(paste(
            "Some grouping keys are not in data.frame returned by the",
            "`residuals()` method. Groupings may not be correct."
          ))
          r <- bind_cols(key_cols, select(r, .resid))
        }
        r <- group_by(r, !!!rlang::syms(common))
      }
    }

    r <- r %>%
      summarize(dstn = quantile_pred(matrix(quantile(
        c(.resid, symmetric * .resid),
        probs = object$quantile_levels, na.rm = TRUE
      ), nrow = 1), quantile_levels = object$quantile_levels))
    # Check for NA
    na_rows <- rowSums(is.na(as.matrix(r$dstn))) > 0
    if (length(common) > 0L && any(na_rows) && !all(na_rows)) {
      bad <- do.call(paste, c(r[na_rows, common], sep = " / "))
      cli_abort(c(
        "Residual quantiles could not be calculated for some keys.",
        x = "All training residuals are missing (`NA`) for: {.val {bad}}.",
        i = paste(
          "This typically happens when a key has too few training observations",
          "for the forecast horizon."
        )
      ))
    }
    if (any(na_rows)) {
      cli_abort(c(
        "Residual quantiles could not be calculated due to missing residuals.",
        i = paste(
          "This can happen when the training window has fewer observations than",
          "the forecast horizon (for example, `n_training` < `ahead` or",
          "`n_train` < `ahead`)."
        )
      ))
    }

    estimate <- components$predictions$.pred
    if (length(common) > 0L) {
      # match each prediction to the residual quantiles of its own key
      keys <- select(components$predictions, all_of(common))
      unseen <- anti_join(distinct(keys), r, by = common)
      if (nrow(unseen) > 0L) {
        unseen_keys <- do.call(paste, c(unseen, sep = " / "))
        cli_warn(c(
          paste(
            "Prediction data contains {cli::qty(length(unseen_keys))}{?a key/keys}",
            "not present in the training data: {.val {unseen_keys}}."
          ),
          i = paste(
            "Because {?this key has/these keys have} no training residuals,",
            "predictive distribution quantiles ({.col {object$name}}) will be `NA`."
          )
        ))
      }
      r <- left_join(keys, r, by = common, relationship = "many-to-one")
    }
    res <- tibble(.pred_distn = r$dstn + estimate)
    res <- check_name(
      res,
      components$predictions,
      object,
      newname = object$name
    )
    components$predictions <- mutate(components$predictions, !!!res)
    components
  }

grab_residuals <- function(the_fit, components) {
  if (the_fit$spec$mode != "regression") {
    cli_abort("For meaningful residuals, the predictor should be a regression model.")
  }
  r <- stats::residuals(the_fit$fit)
  if (!is.null(r)) { # Got something from the method
    cl <- class(the_fit$fit)
    if (inherits(r, "data.frame")) {
      if (".resid" %in% names(r)) { # success
        return(r)
      } else { # failure
        cli_warn(c(
          paste(
            "The `residuals()` method for objects of class {.cls {cl}} returned",
            "a data frame without a column named {.col .resid}."
          ),
          i = paste(
            "Residual quantiles will be calculated directly from the difference",
            "between predictions and observations."
          ),
          i = "This may result in unexpected behavior."
        ))
      }
    } else if (is.vector(drop(r))) { # also success
      return(tibble(.resid = drop(r)))
    } else { # failure
      cli_warn(c(
        paste(
          "The `residuals()` method for objects of class {.cls {cl}} returned",
          "an object that is neither a data frame with a column named",
          "{.col .resid} nor coercible to a vector."
        ),
        i = paste(
          "Residual quantiles will be calculated directly from the difference",
          "between predictions and observations."
        ),
        i = "This may result in unexpected behavior."
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
