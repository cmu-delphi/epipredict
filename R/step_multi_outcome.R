step_multi_outcome <-
  function(recipe,
           ...,
           role = NA,
           trained = FALSE,
           new_key = "original_outcome",
           new_outcome = "outcomes",
           create_sparse = FALSE,
           predictor_suffix = NULL,
           skip = FALSE,
           id = rand_id("multi_outcome")) {
    rlang::check_dots_empty()
    if (!is_epi_recipe(recipe))
      rlang::abort("This recipe step can only operate on an `epi_recipe`.")

    arg_is_chr_scalar(id)
    arg_is_chr(predictor_suffix, allow_null = TRUE)
    arg_is_chr(new_key, new_outcome)
    arg_is_lgl(create_sparse, skip)
    add_step(recipe,
             step_multi_outcome_new(
               role = role,
               trained = trained,
               new_key = new_key,
               new_outcome = new_outcome,
               keys = epi_keys(recipe),
               create_sparse = create_sparse,
               predictor_suffix = predictor_suffix,
               cur_outcomes = NULL,
               cur_predictors = NULL,
               skip = skip,
               id = id
             ))
  }



step_multi_outcome_new <-
  function(role, trained, new_key, new_outcome, keys, create_sparse,
           predictor_suffix, cur_outcomes, cur_predictors,
           skip, id) {
    step(
      subclass = "multi_outcome",
      role = role,
      trained = trained,
      new_key = new_key,
      new_outcome = new_outcome,
      keys = keys,
      create_sparse = create_sparse,
      predictor_suffix = predictor_suffix,
      cur_outcomes = cur_outcomes,
      cur_predictors = cur_predictors,
      skip = skip,
      id = id
    )
  }


#' @export
prep.step_multi_outcome <- function(x, training, info = NULL, ...) {
  outcomes <- recipes_eval_select(all_outcomes(), training, info)
  predictors <- recipes_eval_select(all_predictors(), training, info)
  if (!(length(outcomes) > 1L))
    rlang::abort("Using `step_multi_outcome()` requires at least 2 outcomes.")
  step_multi_outcome_new(
    role = x$role,
    trained = TRUE,
    new_key = x$new_key,
    new_outcome = x$new_outcome,
    keys = x$keys,
    create_sparse = x$create_sparse,
    predictor_suffix = x$predictor_suffix,
    cur_outcomes = outcomes,
    cur_predictors = predictors,
    skip = x$skip,
    id = x$id
  )
}


#' @export
bake.step_multi_outcome <- function(object, new_data, ...) {

  keys <- dplyr::select(new_data, dplyr::all_of(object$keys))
  outcomes <- as.matrix(
    dplyr::select(new_data, dplyr::all_of(object$cur_outcomes)))
  predictors <- as.matrix(
    dplyr::select(new_data, dplyr::all_of(object$cur_predictors)))
  n_outcomes <- length(object$cur_outcomes)
  n_obs <- nrow(keys)
  if (object$create_sparse) {
    Id <- Matrix::Diagonal(n_outcomes)
  } else {
    Id <- diag(nrow = n_outcomes, ncol = n_outcomes)
  }

  new_key <- data.frame(rep(colnames(outcomes), each = n_obs))
  names(new_key) <- object$new_key

  dim(outcomes) <- c(n_outcomes * n_obs, 1)
  colnames(outcomes) <- object$new_outcome

  new_predictors <- kronecker(Id, predictors)
  pred_names <- suffix_creator(object$predictor_suffix,
                               colnames(predictors),
                               n_outcomes)
  colnames(new_predictors) <- paste0(colnames(predictors), pred_names)

  dplyr::bind_cols(
    keys[rep(1:n_obs, times = n_outcomes), ],
    new_key,
    outcomes,
    new_predictors
  )

}

suffix_creator <- function(x, pnames, ntimes) {
  nn <- ntimes * length(pnames)
  x <- x %||% paste0("_", rep(0:(ntimes - 1), each = length(pnames)))
  if (nn > length(x)) {
    x <- paste0(
      rep(x, length.out = nn),
      rep(0:nn, each = length(x), length.out = nn)
    )
  }
  x
}
