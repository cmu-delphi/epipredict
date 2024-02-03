epi_check_training_set <- function(x, rec) {
  # Philosophy, allow the model to be fit with warnings, whenever possible.
  # If geo_type / time_type of the recipe and training data don't match
  #   we proceed and warn.
  # If other_keys is missing from the training set, there are other issues.
  validate_meta_match(x, rec$template, "geo_type", "warn")
  validate_meta_match(x, rec$template, "time_type", "warn")

  # There are 3 possibilities.
  # 1. template has ok that are in x, but not labelled
  # 2. template has ok that are not in x
  # 3. x has ok that are not in template. Not a problem.
  old_ok <- attr(rec$template, "metadata")$other_keys
  new_ok <- attr(x, "metadata")$other_keys

  if (!is.null(old_ok)) {
    if (all(old_ok %in% colnames(x))) { # case 1
      if (!all(old_ok %in% new_ok)) {
        cli::cli_warn(c(
          "The recipe specifies additional keys. Because these are available,",
          "they are being added to the metadata of the training data."
        ))
        attr(x, "metadata")$other_keys <- union(new_ok, old_ok)
      }
    }
    missing_ok <- setdiff(old_ok, colnames(x))
    if (length(missing_ok) > 0) { # case 2
      cli::cli_abort(c(
        "The recipe specifies keys which are not in the training data.",
        i = "The training set is missing columns for {missing_ok}."
      ))
    }
  }
  x
}

validate_meta_match <- function(x, template, meta, warn_or_abort = "warn") {
  new_meta <- attr(x, "metadata")[[meta]]
  old_meta <- attr(template, "metadata")[[meta]]
  msg <- c(
    "The `{meta}` of the training data appears to be different from that",
    "used to construct the recipe. This may result in unexpected consequences.",
    i = "Training `{meta}` is '{new_meta}'.",
    i = "Originally, it was '{old_meta}'."
  )
  if (new_meta != old_meta) {
    switch(warn_or_abort,
      warn = cli::cli_warn(msg),
      abort = cli::cli_abort(msg)
    )
  }
}
