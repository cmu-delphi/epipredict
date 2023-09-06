#' returns both the point estimate and the quantile distribution, regardless of the underlying trainer
#' f
layer_point_and_distn <- function(frosting, trainer, ...,
                                  probs = c(0.05, 0.95),
                                  symmetrize = TRUE,
                                  by_key = character(0L),
                                  distn_name = ".pred_distn",
                                  point_name = NULL,
                                  distn_id = NULL,
                                  point_id = NULL,
                                  point_type = c("median", "mean"),
                                  truncate = c(-Inf, Inf),
                                  use_predictive_distribution = TRUE,
                                  dist_type = "gaussian") {
  rlang::check_dots_empty()
  stopifnot(inherits(recipe, "recipe"))
  # not sure what to do about the dots...
  if (inherits(trainer, "quantile_reg")) {
    tau <- sort(compare_quantile_args(
      args_list$levels,
      rlang::eval_tidy(trainer$args$tau)
    ))
    args_list$levels <- tau
    trainer$args$tau <- rlang::enquo(tau)
    if (is.null(point_id)) {
      point_id <- rand_id("point_from_distn")
    }
    if (is.null(distn_id)) {
      distn_id <- rand_id("quantile_distn")
    }
    frosting %<>% layer_quantile_distn(...,
      levels = tau,
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
      frosting %<>% layer_residual_quantiles(
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
