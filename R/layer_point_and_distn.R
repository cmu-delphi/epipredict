#' returns both the point estimate and the quantile distribution
#' @description
#' This function adds a frosting layer that produces both a point estimate as
#' well as quantile estimates.
#' @param distn_id a random id string for the layer that creates the quantile
#'        estimate
#' @param point_id a random id string for the layer that creates the point
#'        estimate. Only present for trainers that produce quantiles
#' @param point_type character. Either `mean` or `median`.
#' @param use_predictive_distribution only usable for `linear_reg` type models
#' @param distn_type character. Only used if `use_predictive_distribution=TRUE`,
#'               for `linear_reg` type models. Either gaussian or student_t
#' @param distn_name an alternate name for the distribution column; defaults
#'        to `.pred_distn`.
#' @param point_name an alternate name for the point estimate column; defaults
#'        to `.pred`.
#' @param symmetrize logical. If `TRUE` then interval will be symmetric.
#'        Applies for residual quantiles only
#' @param by_key A character vector of keys to group the residuals by before
#'   calculating quantiles. The default, `c()` performs no grouping. Only used
#'   by `layer_residual_quantiles`
#' @inheritParams layer_quantile_distn
#' @export
#' @return an updated `frosting postprocessor` with an additional prediction
#'   column; if the trainer produces a point estimate, it has added a
#'   distribution estimate, and vice versa.
layer_point_and_distn <- function(frosting, trainer, ...,
                                  levels = c(0.25, 0.75),
                                  symmetrize = TRUE,
                                  by_key = character(0L),
                                  distn_name = ".pred_distn",
                                  point_name = NULL,
                                  distn_id = NULL,
                                  point_id = NULL,
                                  point_type = c("median", "mean"),
                                  truncate = c(-Inf, Inf),
                                  use_predictive_distribution = FALSE,
                                  dist_type = "gaussian") {
  rlang::check_dots_empty()
  stopifnot(inherits(recipe, "recipe"))
  levels <- sort(levels)
  if (inherits(trainer, "quantile_reg")) {
    # sort the probabilities
    tau <- sort(compare_quantile_args(
      levels,
      rlang::eval_tidy(trainer$args$tau)
    ))
    levels <- tau
    trainer$args$tau <- rlang::enquo(tau)
    if (is.null(point_id)) {
      point_id <- rand_id("point_from_distn")
    }
    if (is.null(distn_id)) {
      distn_id <- rand_id("quantile_distn")
    }
    frosting %<>% layer_quantile_distn(
      levels = levels,
      truncate = trucate,
      name = distn_name,
      id = distn_id
    ) %>%
      layer_point_from_distn(
        type = point_type,
        name = point_name,
        id = point_id
      )
  } else {
    if (is.null(distn_id)) {
      distn_id <- rand_id("residual_quantiles")
    }
    if (inherits(trainer, "linear_reg") && use_predictive_distribution) {
      frosting %<>% layer_predictive_distn(
        dist_type = dist_type,
        name = distn_name,
        id = distn_id
      )
    } else {
      frosting %<>% layer_residual_quantiles(
        probs = levels,
        symmetrize = symmetrize,
        by_key = by_key,
        name = distn_name,
        id = distn_id
      )
    }
  }
}
