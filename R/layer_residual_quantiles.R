#' Creates predictions based on residual quantiles
#'
#' @param frosting a `frosting` postprocessor
#' @param ... Unused, include for consistency with other layers.
#' @param probs numeric vector of probabilities with values in (0,1)
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
#' wf <- epi_workflow(r, parsnip::linear_reg()) %>%
#'  parsnip::fit(jhu)
#'
#' latest <- get_test_data(recipe = r, x = jhu)
#'
#' f <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles(probs = c(0.0275, 0.975), symmetrize = FALSE) %>%
#'   layer_naomit(.pred)
#' wf1 <- wf %>% add_frosting(f)
#'
#' p <- predict(wf1, latest)
#'
#' f2 <- frosting() %>%
#'   layer_predict() %>%
#'   layer_residual_quantiles(probs = c(0.3, 0.7), by_key = "geo_value") %>%
#'   layer_naomit(.pred)
#' wf2 <- wf %>% add_frosting(f2)
#'
#' p2 <- predict(wf2, latest)
layer_residual_quantiles <- function(frosting, ...,
                                     probs = c(0.05, 0.95),
                                     symmetrize = TRUE,
                                     by_key = character(0L),
                                     name = ".pred_distn",
                                     id = rand_id("residual_quantiles")) {
  rlang::check_dots_empty()
  arg_is_scalar(symmetrize)
  arg_is_chr_scalar(name, id)
  arg_is_chr(by_key, allow_null = TRUE)
  arg_is_probabilities(probs)
  arg_is_lgl(symmetrize)
  add_layer(
    frosting,
    layer_residual_quantiles_new(
      probs = probs,
      symmetrize = symmetrize,
      by_key = by_key,
      name = name,
      id = id
    )
  )
}

layer_residual_quantiles_new <- function(probs, symmetrize, by_key, name, id) {
  layer("residual_quantiles", probs = probs, symmetrize = symmetrize,
        by_key = by_key, name = name, id = id)
}

#' @export
slather.layer_residual_quantiles <-
  function(object, components, the_fit, the_recipe, ...) {
    if (is.null(object$probs)) return(components)


    s <- ifelse(object$symmetrize, -1, NA)
    r <- dplyr::bind_cols(
      r = grab_residuals(the_fit, components),
      geo_value = components$mold$extras$roles$geo_value,
      components$mold$extras$roles$key)

    ## Handle any grouping requests
    if (length(object$by_key) > 0L) {
      common <- intersect(object$by_key, names(r))
      excess <- setdiff(object$by_key, names(r))
      if (length(excess) > 0L) {
        cli_warn("Requested residual grouping key(s) {excess} unavailable ",
            "in the original data. Grouping by the remainder {common}.")

      }
      if (length(common) > 0L)
        r <- r %>% dplyr::group_by(!!!rlang::syms(common))
    }

    r <- r %>%
      dplyr::summarise(
        q = list(quantile(c(r, s * r), probs = object$probs, na.rm = TRUE))
      )

    estimate <- components$predictions$.pred
    res <- tibble::tibble(
      .pred_distn = dist_quantiles(map2(estimate, r$q, "+"), object$probs)
    )
    res <- check_pname(res, components$predictions, object)
    components$predictions <- dplyr::mutate(components$predictions, !!!res)
    components
  }

grab_residuals <- function(the_fit, components) {
  if (the_fit$spec$mode != "regression")
    rlang::abort("For meaningful residuals, the predictor should be a regression model.")
  r_generic <- attr(utils::methods(class = class(the_fit)[1]), "info")$generic
  if ("residuals" %in% r_generic) {
    r <- residuals(the_fit)
  } else {
    yhat <- predict(the_fit, new_data = components$mold$predictors)
    r <- c(components$mold$outcomes - yhat)[[1]]
  }
  r
}
